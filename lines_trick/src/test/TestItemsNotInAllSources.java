package test;

import java.util.ArrayList;

import main.ItemsNotInAllSources;

public class TestItemsNotInAllSources {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {

			tstEmptySourcesList();
			tstEmptySources();
			tst2SameItemsInSource();
			tstNoSameItemsInDifferentSources();
			tstAllItemsInAllSources();
			tst2ItemsInAllAnd2ItemsNotInAll();
			System.out.println("TestItemsNotInAllSources: Success!");

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("TestItemsNotInAllSources: Failed!");
		}
	}

	private static void tstEmptySourcesList() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().isEmpty());
	}

	private static void tstEmptySources() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		sources.add(new ArrayList<Integer>());
		sources.add(new ArrayList<Integer>());
		sources.add(new ArrayList<Integer>());
		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().isEmpty());
	}

	private static void tst2SameItemsInSource() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		ArrayList<Integer> src1 = new ArrayList<Integer>();
		ArrayList<Integer> src2 = new ArrayList<Integer>();
		sources.add(src1);
		sources.add(src2);
		src1.add(1);
		src1.add(1);
		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().size() == 1);
		assert(filter.getItems().get(0).equals(1));
	}

	private static void tstNoSameItemsInDifferentSources() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		ArrayList<Integer> src1 = new ArrayList<Integer>();
		ArrayList<Integer> src2 = new ArrayList<Integer>();
		ArrayList<Integer> src3 = new ArrayList<Integer>();
		sources.add(src1);
		sources.add(src2);
		sources.add(src3);
		src1.add(1);
		src2.add(2);
		src3.add(3);
		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().size() == 3);
		assert(filter.getItems().contains(new Integer(1)));
		assert(filter.getItems().contains(new Integer(2)));
		assert(filter.getItems().contains(new Integer(3)));
	}

	@SuppressWarnings("unchecked")
	private static void tstAllItemsInAllSources() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		ArrayList<Integer> src = new ArrayList<Integer>();
		src.add(1);
		src.add(2);
		src.add(3);
		sources.add(src);
		sources.add((ArrayList<Integer>)src.clone());
		sources.add((ArrayList<Integer>)src.clone());

		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().isEmpty());
	}

	@SuppressWarnings("unchecked")
	private static void tst2ItemsInAllAnd2ItemsNotInAll() {
		ArrayList<Iterable<Integer>> sources = new ArrayList<Iterable<Integer>>();
		ArrayList<Integer> src1 = new ArrayList<Integer>();
		src1.add(1);
		src1.add(2);
		ArrayList<Integer> src2 = (ArrayList<Integer>)src1.clone();
		sources.add(src1);
		sources.add(src2);

		src1.add(3);
		src2.add(4);

		ItemsNotInAllSources<Integer> filter = new ItemsNotInAllSources<Integer>(sources);
		assert(filter.getItems().size() == 2);
		assert(filter.getItems().contains(new Integer(3)));
		assert(filter.getItems().contains(new Integer(4)));
	}
}
