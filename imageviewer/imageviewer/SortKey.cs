using System;

namespace ImageViewer
{
	public enum SortKeys
	{
		Name, Extension, Size, Date
	}

	public class SortKey
	{
		private SortKey ()
		{
			Key = SortKeys.Name;
		}

		public static SortKey Instance
		{
			get
			{
				if (instance == null)
					instance = new SortKey();
				return instance;
			}
		}

		public SortKeys Key
		{
			get;
			set;
		}

		private static SortKey instance;
	}
}

