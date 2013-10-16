package main;

import java.util.ArrayList;
import java.util.List;

public class StringsNotInAllFiles {

	public static List<String> getStrings(List<String> files) {
		ArrayList<Iterable<String>> sources =
				new ArrayList<Iterable<String>>(files.size());
		for (String fileName : files)
			sources.add(new FileStringsRdOnlyIterator(fileName));

		ItemsNotInAllSources<String> filtered = new ItemsNotInAllSources<String>(sources);

		for (Iterable<String> iter : sources)
			((FileStringsRdOnlyIterator)iter).close();
		return filtered.getItems();
	}

	/**
	 * @param files
	 */
	public static void main(String[] files) {
		ArrayList<String> filesList = new ArrayList<String>(files.length);
		for (String file : files)
			filesList.add(file);
		for (String str : getStrings(filesList))
			System.out.println(str);
	}

}
