using System;
using System.IO;

namespace ImageViewer
{
	public class ItemData : IComparable<ItemData>
	{
		public ItemData () {  }

		public ItemData (FileInfo fi)
		{
			Name = fi.Name;
			Extension = fi.Extension;
			FullName = fi.FullName;
			Size = fi.Length;
			Date = fi.LastWriteTimeUtc;
			IsFile = true;
		}

		public ItemData (DirectoryInfo di)
		{
			Name = di.Name;
			Extension = string.Empty;
			FullName = di.FullName;
			Size = 0;
			Date = di.LastWriteTimeUtc;
			IsFile = false;
		}

		public int CompareTo(ItemData obj)
		{
			if (!IsFile && obj.IsFile)
				return 1;
			if (IsFile && !obj.IsFile)
				return -1;

			int result = 0;

			switch (SortKey.Instance.Key)
			{
			case SortKeys.Name:
				result = string.Compare(Name, obj.Name, false);
				break;
			case SortKeys.Extension:
				int extcmp = string.Compare(Extension, obj.Extension, false);
				if (extcmp != 0)
					result = extcmp;
				else
					result = string.Compare(Name, obj.Name);
				break;
			case SortKeys.Size:
				result = Size.CompareTo(obj.Size);
				break;
			case SortKeys.Date:
				result = Date.CompareTo(obj.Date);
				break;
			}

			return result;
		}

		public string Name { get; set; }
		public string Extension { get; set; }
		public string FullName { get; set; }
		public long Size { get; set; }
		public DateTime Date { get; set; }
		public bool IsFile { get; set; }
	}
}

