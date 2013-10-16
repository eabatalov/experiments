package main;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ItemsNotInAllSources<T> {
	
	public ItemsNotInAllSources(List<Iterable<T>> sources) {
		this.items = new ArrayList<T>();
		HashMap<T, ItemInfo> knownItems = new HashMap<T, ItemInfo>();

		for (int sourceIx = 0; sourceIx < sources.size(); ++sourceIx) {
			Iterable<T> source = sources.get(sourceIx);
			for (T item : source) {
					if (knownItems.containsKey(item))
						knownItems.get(item).IncOccurenceIfNeeded(sourceIx);
					else
						knownItems.put(item, new ItemInfo(sourceIx));
			}
		}

		for (T item : knownItems.keySet())
			if (knownItems.get(item).getOccurenceCnt() < sources.size())
				items.add(item);
	}

	public List<T> getItems() {
		return items;
	}

	private final ArrayList<T> items;

	private class ItemInfo {
	
		public ItemInfo(int sourceIx) {
			lastIncSourceIx = sourceIx;
			occurenceCnt = 1;
		}
		
		public void IncOccurenceIfNeeded(int sourceIx) {
			if (sourceIx != lastIncSourceIx)
				++occurenceCnt;
			lastIncSourceIx = sourceIx;
		}
		
		public int getOccurenceCnt() {
			return occurenceCnt;
		}
		
		private int occurenceCnt;
		private int lastIncSourceIx; 
	}
}