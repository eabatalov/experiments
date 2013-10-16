package main;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;


public class FileStringsRdOnlyIterator implements Iterator<String>, Iterable<String>  {

	public FileStringsRdOnlyIterator(String fileName) {

		try {
			fileReader = new BufferedReader(new FileReader(fileName));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return;
		}

		try {
			nextStr = fileReader.readLine();
		} catch (IOException e) {
			e.printStackTrace();
			close();
			return;
		}
	}

	public void close() {
		if (fileReader != null) {
			try {
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				fileReader = null;
				nextStr = null;
			}
		}
	}
	
	@Override
	public boolean hasNext() {
		return nextStr != null;
	}

	@Override
	public String next() {
		if (fileReader != null && nextStr != null) {
			String result = nextStr;
			nextStr = readLineSafe();
			return result;
		} else throw new NoSuchElementException();
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException("This iterator is read only");
	}

	@Override
	public Iterator<String> iterator() {
		return this;
	}

	private String readLineSafe() {
		String result = null;
		try {
			result = fileReader.readLine();
		} catch (IOException e) {
			e.printStackTrace();
			close();
		}
		return result;
	}

	private BufferedReader fileReader;
	private String nextStr;
}