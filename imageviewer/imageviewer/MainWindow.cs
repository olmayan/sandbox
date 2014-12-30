using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Gtk;

namespace ImageViewer
{
	public class MainWindow : Window
	{
		public MainWindow () : base("Image Viewer")
		{
			Destroyed += delegate {
				if (bkgproc != null && bkgproc.IsAlive)
					bkgproc.Abort();
				Application.Quit();
			};

			MenuBar menubar = new MenuBar();
			Menu menu;

			MenuItem viewmenu = new MenuItem("_View");
			menu = new Menu();
			viewmenu.Submenu = menu;
			menubar.Add(viewmenu);

			MenuItem sort = new MenuItem("_Sort by");
			Menu sortmenu = new Menu();
			sort.Submenu = sortmenu;
			menu.Add(sort);

			RadioMenuItem byname = new RadioMenuItem("_Name");
			byname.Toggled += delegate(object sender, EventArgs e) {
				SortKey.Instance.Key = SortKeys.Name;
				list.SetSortFunc(2, SortFunc);
			};
			sortmenu.Add(byname);

			RadioMenuItem byext = new RadioMenuItem(byname, "_Extension");
			byext.Toggled += delegate(object sender, EventArgs e) {
				SortKey.Instance.Key = SortKeys.Extension;
				list.SetSortFunc(2, SortFunc);
			};
			sortmenu.Add(byext);

			RadioMenuItem bysize = new RadioMenuItem(byname, "_Size");
			bysize.Toggled += delegate(object sender, EventArgs e) {
				SortKey.Instance.Key = SortKeys.Size;
				list.SetSortFunc(2, SortFunc);
			};
			sortmenu.Add(bysize);

			RadioMenuItem bydate = new RadioMenuItem(byname, "_Date");
			bydate.Toggled += delegate(object sender, EventArgs e) {
				SortKey.Instance.Key = SortKeys.Date;
				list.SetSortFunc(2, SortFunc);
			};
			sortmenu.Add(bydate);

			addr = new Entry();
			addr.KeyReleaseEvent += delegate(object o, KeyReleaseEventArgs args) {
				if (args.Event.Key == Gdk.Key.Return)
				{
					LoadFolder(addr.Text, addr);
				}
			};

			list = new ListStore(typeof(string), typeof(Gdk.Pixbuf), typeof(ItemData));
			list.SetSortColumnId(2, SortType.Ascending);
			list.SetSortFunc(2,  SortFunc);

			view = new IconView(list);
			view.TextColumn = 0;
			view.PixbufColumn = 1;
			view.ItemWidth = iconsize + 32;
			view.ItemActivated += delegate(object o, ItemActivatedArgs args) {
				TreeIter iter;
				list.GetIter(out iter, args.Path);
				ItemData itemData = (ItemData)list.GetValue(iter, 2);
				if (itemData.IsFile)
					return;
				LoadFolder(itemData.FullName);
			};
			view.KeyReleaseEvent += delegate(object o, KeyReleaseEventArgs args) {
				if (args.Event.Key == Gdk.Key.BackSpace)
				{
					if (curdir.Root.FullName == curdir.FullName)
						return;
					LoadFolder(curdir.Parent.FullName);
					view.Activate();
				}
			};

			ScrolledWindow scrolled = new ScrolledWindow();
			scrolled.HscrollbarPolicy = PolicyType.Automatic;
			scrolled.VscrollbarPolicy = PolicyType.Automatic;
			scrolled.Add(view);

			statusbar = new Statusbar();
			//layout.Add(statusbar);
			//progressbar = new ProgressBar();
			//layout.Add(progressbar);

			Box box = new VBox();
			box.Homogeneous = false;
			box.PackStart(menubar, false, false, 0);
			box.PackStart(addr, false, false, 0);
			box.PackStart(scrolled, true, true, 0);
			box.PackStart(statusbar, false, false, 0);

			Add(box);

			SetSizeRequest(640, 480);

			icon = RenderIconPixbuf(Stock.Directory, IconSize.Dialog);
			fileIcon = RenderIconPixbuf(Stock.File, IconSize.Dialog);
			mimeicons = new Dictionary<string, Gdk.Pixbuf>();
			noicons = new List<string>();
			bkgproc = null;

			box.FocusChain = new Widget[] { scrolled, addr };

			string path = (
				System.Environment.OSVersion.Platform == PlatformID.MacOSX ||
				System.Environment.OSVersion.Platform == PlatformID.Unix)
				? Environment.GetEnvironmentVariable("HOME")
				: Environment.GetEnvironmentVariable("HOMEDRIVE") + Environment.GetEnvironmentVariable("HOMEPATH");
			LoadFolder(path);
		}

		public void LoadFolder(string pathname, Widget focus = null)
		{
			addr.Sensitive = view.Sensitive = false;
			int selStart, selEnd;
			addr.GetSelectionBounds(out selStart, out selEnd);

			Thread thread = new Thread(new ThreadStart(delegate {
				DirectoryInfo dir = (pathname == string.Empty) ? null : new DirectoryInfo(pathname);

				List<DirectoryInfo> dirInfos;
				List<FileInfo> fileInfos = null;

				ManualResetEvent e = new ManualResetEvent(false);

				if (dir == null || !dir.Exists)
				{
					Application.Invoke(delegate {
						MessageDialog dlg = new MessageDialog(this, DialogFlags.Modal, MessageType.Warning, ButtonsType.Ok, "Directory does not exist!");
						dlg.Title = "Error";
						dlg.Run();
						dlg.Destroy();
						e.Set();
					} );
					e.WaitOne();
				}
				else
				{
					if (bkgproc != null && bkgproc.IsAlive)
						bkgproc.Abort();

					dirInfos = Directory.GetDirectories(pathname).ToList()
						.Select(s => new DirectoryInfo(s)).Where(di => (di.Attributes & FileAttributes.Hidden) == 0).ToList();
					List<object[]> items = dirInfos.Select(di => new object[] { 
						di.Name,
						icon,
						new ItemData(di) } ).ToList();

					fileInfos = Directory.GetFiles(pathname).ToList()
						.Select(s => new FileInfo(s)).Where(fi => (fi.Attributes & FileAttributes.Hidden) == 0).ToList();
					items.AddRange(fileInfos.Select(fi => new object[] { 
						fi.Name,
						fileIcon,//GetMimeIcon(fi.FullName),
						new ItemData(fi) } ).ToList());

					Application.Invoke(delegate { 
						list.Clear();
						foreach (object[] values in items)
							list.AppendValues(values);
						addr.Text = pathname;
						e.Set();
					} );
					e.WaitOne();
					curdir = dir;
				}

				e.Reset();
				Application.Invoke(delegate {
					addr.Sensitive = view.Sensitive = true;
					((focus == null) ? view : focus).GrabFocus();
					if (focus == addr) addr.SelectRegion(selStart, selEnd);
					e.Set();
				} );
				e.WaitOne();

				if (fileInfos != null && fileInfos.Count > 0)
				{
					bkgproc = new Thread(new ThreadStart(delegate { LoadMimeIcons(); } ));
					bkgproc.Start();
				}
			} ));

			thread.Start();
		}

		void LoadMimeIcons()
		{
			Dictionary<TreePath, string> files = new Dictionary<TreePath, string>();

			TreeIter iter;
			ManualResetEvent e = new ManualResetEvent(false);
			Application.Invoke(delegate {
				if (list.GetIterFirst(out iter))
				{
					do
					{
						ItemData data = (ItemData)list.GetValue(iter, 2);
						if (data.IsFile)
							files.Add(list.GetPath(iter), data.FullName);
					} while (list.IterNext(ref iter));
				}
				e.Set();
			} );
			e.WaitOne();

			Dictionary<TreePath, Gdk.Pixbuf> pixbufs = new Dictionary<TreePath, Gdk.Pixbuf>();
			files.Keys.ToList().ForEach(p => pixbufs.Add(p, GetMimeIcon(files[p])));

			e.Reset();
			Application.Invoke(delegate {
				pixbufs.Keys.ToList().ForEach(p => {
					list.GetIter(out iter, p);
					list.SetValue(iter, 1, pixbufs[p]);
				} );

				e.Set();
			} );
			e.WaitOne();

			bkgproc = new Thread(new ThreadStart(delegate { LoadImages(); } ));
			bkgproc.Start();
		}

		void LoadImages()
		{
			Dictionary<TreePath, string> files = new Dictionary<TreePath, string>();

			TreeIter iter;
			ManualResetEvent e = new ManualResetEvent(false);
			Application.Invoke(delegate {
				if (list.GetIterFirst(out iter))
				{
					do
					{
						ItemData data = (ItemData)list.GetValue(iter, 2);
						if (data.IsFile && data.Size < 10 * 1024 * 1024)
							files.Add(list.GetPath(iter), data.FullName);
					} while (list.IterNext(ref iter));
				}
				e.Set();
			} );
			e.WaitOne();

			Dictionary<TreePath, Gdk.Pixbuf> pixbufs = new Dictionary<TreePath, Gdk.Pixbuf>();
			int i = 0, n = files.Keys.Count, percentage = 0, cur_percentage = 0;
			files.Keys.ToList().ForEach(p => {
				try
				{
					pixbufs.Add(p, new Gdk.Pixbuf(files[p], iconsize, iconsize, true));
				}
				catch (Exception) {  }

				i++;
				cur_percentage = i * 100 / n;
				if (cur_percentage != percentage)
				{
					percentage = cur_percentage;
					e.Reset();
					Application.Invoke(delegate {
						statusbar.Pop(0);
						statusbar.Push(0, string.Format("Generating thumbnails ({0:d}%)", percentage));
						e.Set();
					} );
					e.WaitOne();
				}
			} );

			e.Reset();
			Application.Invoke(delegate {
				pixbufs.Keys.ToList().ForEach(p => {
					list.GetIter(out iter, p);
					list.SetValue(iter, 1, pixbufs[p]);
					//statusbar.Push(0, string.Format("Generating thumbnails ({0:d}%)", i * 100 / n));
				} );

				statusbar.Pop(0);
				if (pixbufs.Keys.Count > 0) statusbar.Push(0, "Images thumbnails generated");
			} );
			e.WaitOne();

			/*Application.Invoke(delegate {
				statusbar.Push(0, "Images thumbnails generated");
			} );*/
		}

		Gdk.Pixbuf GetMimeIcon(string filename)
		{
			byte data;
			bool uncertain;

			string ctype = GLib.ContentType.Guess(filename, out data, 0, out uncertain);

			if (uncertain || noicons.Exists(s => s == ctype))
				return fileIcon;

			if (mimeicons.ContainsKey(ctype))
				return mimeicons[ctype];
				
			IconInfo info = IconTheme.Default.LookupIcon(GLib.ContentType.GetIcon(ctype), iconsize, IconLookupFlags.GenericFallback);
			Gdk.Pixbuf pixbuf;
			try
			{
				pixbuf = info.LoadIcon();
			}
			catch (Exception)
			{
				pixbuf = null;
			}

			if (pixbuf == null)
				return fileIcon;

			mimeicons.Add(ctype, pixbuf);
			return pixbuf;
		}

		int SortFunc(ITreeModel model, TreeIter a0, TreeIter b0)
		{
			ItemData a = (ItemData)model.GetValue(a0, 2);
			ItemData b = (ItemData)model.GetValue(b0, 2);
			if (a == null)
				return (b == null) ? 0 : -1;
			if (b == null)
				return 1;

			if (a.IsFile && !b.IsFile)
				return 1;
			if (!a.IsFile && b.IsFile)
				return -1;

			int result = 0;

			switch (SortKey.Instance.Key)
			{
			case SortKeys.Name:
				result = string.Compare(a.Name, b.Name, false);
				break;
			case SortKeys.Extension:
				int extcmp = string.Compare(a.Extension, b.Extension, false);
				result = (extcmp == 0) ? string.Compare(a.Name, b.Name, false) : extcmp;
				break;
			case SortKeys.Size:
				result = a.Size.CompareTo(b.Size);
				break;
			case SortKeys.Date:
				result = a.Date.CompareTo(b.Date);
				break;
			}
			return result;
		}

		int iconsize = 48;
		Entry addr;
		DirectoryInfo curdir;
		ListStore list;
		IconView view;
		Gdk.Pixbuf icon, fileIcon;
		Statusbar statusbar;
		//ProgressBar progressbar;
		Dictionary<string, Gdk.Pixbuf> mimeicons;
		List<string> noicons;
		Thread bkgproc;
	}
}

