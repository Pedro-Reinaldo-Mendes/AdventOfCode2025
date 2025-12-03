import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedReader;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 3 > Puzzle 1.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.0
 */
public class D3Puzzle1 {

    /**
     * Main method to obtain the solution of the puzzle given a certain
     * well-formatted input in "input.txt" file.
     * 
     * @param args Command-line arguments (unused on our application).
     * @throws IOException if the required "input.txt" was not found on the same
     *                     directory as where the execution command is done.
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
    private static int solvePuzzle(BufferedReader br) {

        String line;
        int result = 0; // Even though when initialized by default, the value is 0

        try {
            // We'll go through each line of the input
            while ((line = br.readLine()) != null) {
                char[] highestDigits = highestDigits(line);
                // We'll, for each line, sum to our result the largest possible "joltage"
                result += (highestDigits[0] - '0') * 10 + (highestDigits[1] - '0');
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        return result;
    }

    /**
     * Returns the two digits referred to the batteries we must switch on in our
     * puzzle to get the highest joltage possible.
     * 
     * @param line The battery bank we want to analyze.
     * @return the two digits referred to the batteries we must switch on.
     * @throws NullPointerException if {@code line == null}.
     * @implSpec the {@code line} must be a well-formatted battery pack as describe
     *           in the project intructions.
     */
    private static char[] highestDigits(String line) {

        char highNum = line.charAt(0);
        int iOfHighNum = 0;
        boolean moreThanOneHighNum = false; // We'll use this flag to indicate the highest number is repeated
        char secondPossibleHighNum = '0';

        // We'll first obtain our first digit
        for (int i = 1; i < line.length() - 1; i++) {
            // Our first digit must be always the highest one
            if (line.charAt(i) > highNum) {
                moreThanOneHighNum = false;
                highNum = line.charAt(i);
                iOfHighNum = i;
            } else if (line.charAt(i) == highNum)
                moreThanOneHighNum = true;
        }

        // If the highest number appears more than once the result will be the same
        // digit twice or that digit with the last possible char
        if (moreThanOneHighNum)
            return new char[] { highNum,
                    (line.charAt(line.length() - 1) > highNum) ? line.charAt(line.length() - 1) : highNum };

        // Now, to get the second digit, we simply need to run a for loop across every
        // digit after the highest one
        for (int j = iOfHighNum + 1; j < line.length(); j++) {
            if (line.charAt(j) > secondPossibleHighNum)
                secondPossibleHighNum = line.charAt(j);
        }

        // We finally return our highest possible "joltage"
        return new char[] { highNum, secondPossibleHighNum };
    }

}