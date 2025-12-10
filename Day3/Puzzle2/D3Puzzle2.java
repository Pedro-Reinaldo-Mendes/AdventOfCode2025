import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedReader;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 3 > Puzzle 2.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.2
 */
public class D3Puzzle2 {

    /**
     * Main method to obtain the solution of the puzzle given a certain
     * well-formatted input in "input.txt" file.
     * 
     * @param args Command-line arguments (unused on our application).
     * @see #solvePuzzle(BufferedReader)
     * @implSpec The input must be well-formatted as explained in the puzzle's
     *           instructions.
     */
    public static void main(String[] args) {

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            System.out.println(solvePuzzle(br));
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }

    /**
     * Returns the final solution of our puzzle.
     * 
     * @param br The input reader already well-configured.
     * @return the final solution of our puzzle.
     * @throws NullPointerException if {@code br == null}.
     * @see #highestDigits(String)
     * @implSpec {@code br} must be well-configured and linked to a valid input as
     *           explained in the puzzle's instructions.
     */
    private static long solvePuzzle(BufferedReader br) {

        String line;
        long result = 0; // Even though when initialized by default, the value is 0

        char[] highestDigits;

        try {
            // We'll go through each line of the input
            while ((line = br.readLine()) != null) {
                highestDigits = highestDigits(line);
                // We'll, for each line, sum to our result the largest possible "joltage"
                for (int i = highestDigits.length - 1; i >= 0; i--) {
                    result += (highestDigits[highestDigits.length - 1 - i] - '0') * (long) Math.pow(10, i);

                    System.out.println(
                            " --\n" + highestDigits[highestDigits.length - 1 - i] + "\n" + line + "\n" + result);
                }
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        return result;
    }

    /**
     * We'll create this record in order to store a char-int pair easily.
     * 
     * @implNote We're using a record instead of a Class because we're talking about
     *           only two attributes and we're believing in a faster garbage
     *           collector's speed over the overhead of safely managing a standard
     *           class's mutability.
     */
    record CharAndIntPair(char c, int i) {
    }

    /**
     * Returns the twelve digits referred to the batteries we must switch on in our
     * puzzle to get the highest joltage possible.
     * 
     * @param line The battery bank we want to analyze.
     * @return the twelve digits referred to the batteries we must switch on.
     * @throws NullPointerException if {@code line == null}.
     * @see #highestNumAndIndex(String, int, int)
     * @implSpec the {@code line} must be a well-formatted battery pack as describe
     *           in the project intructions.
     */
    private static char[] highestDigits(String line) {

        // Our result sequence of 12 digits
        char[] result = new char[12];

        int beginIndex = 0;
        CharAndIntPair highNumAndIndex;

        // For better speed (not sure if javac automatically arranges this for us on the
        // bytecode...) and simplicity we'll store already the line length because it'll
        // be used a lot on our next iterations
        int lineLength = line.length();

        // We'll go through each possible segment of the full sequence to find the
        // highest digit
        for (int endIndex = lineLength - 11; endIndex <= lineLength; endIndex++) {
            highNumAndIndex = highestNumAndIndex(line, beginIndex, endIndex); // We'll retrieve both the digit
            // and its index
            result[endIndex - lineLength + 11] = highNumAndIndex.c();
            beginIndex = highNumAndIndex.i() + 1;
        }

        // We finally return our result
        return result;
    }

    /**
     * Returns the highest digit, as a char, of a well-formatted char sequence (aka
     * String) together with its correspondent index on the sequence.
     * 
     * @param str        The char sequence we want to analyze.
     * @param beginIndex The inclusive begining index of our search interval.
     * @param endIndex   The exclusive ending index of our search interval.
     * @return the highest digit, as a char, with its correspondent index on the
     *         char sequence.
     * @throws NullPointerException if {@code str == null}.
     * @implSpec {@code str} must be well-formatted as indicated in the puzzle's
     *           instructions.
     * @implSpec {@code beginIndex} is inclusive.
     * @implSpec {@code endIndex} is exclusive.
     */
    private static CharAndIntPair highestNumAndIndex(String str, int beginIndex, int endIndex) {

        char highNum = str.charAt(beginIndex);
        int highNumIndex = beginIndex;

        for (int i = beginIndex + 1; i < endIndex; i++) {
            // We'll always prefer the highest digit at the most left possible, therefore we
            // use ">" instead of ">="
            if (str.charAt(i) > highNum) {
                highNum = str.charAt(i);
                highNumIndex = i;
            }
        }

        return new CharAndIntPair(highNum, highNumIndex);
    }

}