using System;
using Gtk;

namespace ImageViewer
{
	public class ImageViewer
	{
		public static void Main(string[] args)
		{
			Application.Init("ImageViewer", ref args);
			MainWindow window = new MainWindow();
			window.ShowAll();
			Application.Run();
		}
	}
}
