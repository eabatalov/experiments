package test;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import main.FileStringsRdOnlyIterator;

public class TestFileStringRdOnlyIterator {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {

			tstInvalidFile();
			tstNoExceptionsAfterFileDeletion();
			tst5Lines();
			tstStringsContents();
			System.out.println("TestFileStringRdOnlyIterator: Success!");

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("TestFileStringRdOnlyIterator: Failed!");
		}
	}

	private static void tstInvalidFile() {
		FileStringsRdOnlyIterator iter = null;
		File f = null;

		try {		
			f = new File("tstInvalidFile");
			f.mkdir();
			iter = new FileStringsRdOnlyIterator(f.getAbsolutePath());
			assert(!iter.hasNext());
		} finally {
			cleanupIteratorAndFile(iter, f);
		}
	}

	private static void tstNoExceptionsAfterFileDeletion() throws IOException {
		FileStringsRdOnlyIterator iter = null;
		File f = null;
		
		try {
			f = mk5LinesFile();
			iter = new FileStringsRdOnlyIterator(f.getAbsolutePath());
			f.delete();
			while(iter.hasNext())
				iter.next();
			assert(true);
		} finally {
			cleanupIteratorAndFile(iter, f);
		}
	}

	private static void tst5Lines() throws IOException {
		FileStringsRdOnlyIterator iter = null;
		File f = null;

		try {
			f= mk5LinesFile();
			iter = new FileStringsRdOnlyIterator(f.getAbsolutePath());
			iter.next();
			iter.next();
			iter.next();
			iter.next();
			iter.next();
			assert(!iter.hasNext());
		} finally {
			cleanupIteratorAndFile(iter, f);
		}
	}

	private static void tstStringsContents() throws IOException {
		FileStringsRdOnlyIterator iter = null;
		File f = null;
		
		try {
			f= mk5LinesFile();
			iter = new FileStringsRdOnlyIterator(f.getAbsolutePath());
			assert(TEST_FILE_LINE1.equals(iter.next()));
			assert(TEST_FILE_LINE2.equals(iter.next()));
			assert(TEST_FILE_LINE3.equals(iter.next()));
			assert(TEST_FILE_LINE4.equals(iter.next()));
			assert(TEST_FILE_LINE5.equals(iter.next()));
		} finally {
			cleanupIteratorAndFile(iter, f);
		}
	}

	private static String TEST_FILE_LINE1 = "Line 1";
	private static String TEST_FILE_LINE2 = "Line 2";
	private static String TEST_FILE_LINE3 = "Line 3";
	private static String TEST_FILE_LINE4 = "Line 4";
	private static String TEST_FILE_LINE5 = "Line 5";

	private static File mk5LinesFile() throws IOException {
		File f = File.createTempFile("5lines", null);
		PrintWriter out = new PrintWriter(f.getAbsolutePath()); 
		out.println(TEST_FILE_LINE1);
		out.println(TEST_FILE_LINE2);
		out.println(TEST_FILE_LINE3);
		out.println(TEST_FILE_LINE4);
		out.println(TEST_FILE_LINE5);
		out.close();
		return f;
	}

	private static void cleanupIteratorAndFile(FileStringsRdOnlyIterator iter, File f)
	{
		if (iter != null)
			iter.close();
		if (f != null && f.exists())
			f.delete();
	}
}