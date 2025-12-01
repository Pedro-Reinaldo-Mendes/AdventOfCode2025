import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 1 > Puzzle 1.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.0
 */
public class Puzzle1 {

    /**
     * Main function to obtain and **print** the result (password) of the puzzle,
     * given a certain well-formatted input in "input.txt" file.
     * 
     * @param args Command-line arguments (unused in this application).
     * @throws FileNotFoundException if the required input file "input.txt" is not
     *                               found in the same working directory as the
     *                               execution command.
     * @see #calculatePassword(Scanner)
     * @implSpec This function reads the input from "input.txt". **The file's
     *           content must be well-formatted as indicated in the problem
     *           instructions**.
     * @implSpec The number of rotations in each step of the input file must be less
     *           than or equal to {@code Integer.MAX_VALUE - cursor} to prevent
     *           integer overflow.
     */
    public static void main(String[] args) {

        try (Scanner sc = new Scanner(new File("input.txt"))) {
            // We'll simply call a method to calculate the password and then we'll print it
            System.out.println(calculatePassword(sc));
        } catch (FileNotFoundException e) {
            // If the file was not found, we'll return the error message
            System.err.println(e.getMessage());
        }

    }

    /**
     * Returns the password following the puzzle's instructions, given an already
     * configured {@code Scanner}.
     * 
     * @param sc Scanner already configured to read the input file.
     * @return the password following the puzzle's intructions.
     * @throws NullPointerException if {@code sc == null}.
     * @see #calculateDirection(char)
     * @implSpec The desired input file will be read from {@code sc}. **{@code sc}
     *           must be correctly configured to read the desired
     *           input file.**
     * @implSpec **the input file must be well-formatted as indicated in the
     *           problem instructions.**
     * @implSpec **the number of rotations in each step of the input file must be
     *           less than or equal to {@code Integer.MAX_VALUE - cursor}.**
     */
    private static int calculatePassword(Scanner sc) {

        int password = 0; // We'll initialize our result variable (password)
        int cursor = 50; // As specified in the instructions, the cursor starts at position 50

        // For each line in the input file
        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            int direction = calculateDirection(line.charAt(0)); // Direction will be 1 if the rotation is to the right
                                                                // and -1 if the rotation is to the left
            int jumpDistance = Integer.parseInt(line.substring(1));
            cursor = (cursor + direction * jumpDistance) % 100;

            if (cursor == 0)
                password++;
        }

        // We'll finally return the password
        return password;
    }

    /**
     * Returns 1 if the direction is "right" (represented by a char 'R') or returns
     * -1 if the direction is "left" (represented by a char 'L').
     * 
     * @param direction The char that describes the direction.
     * @return {@code 1} if {@code direction == 'R'} or {@code -1} if
     *         {@code direction == 'L'}.
     * @implSpec **the direction must be represented as the char 'R' if "right" or
     *           'L' if "left", there's no other possibility.**
     */
    private static int calculateDirection(char direction) {
        return (direction == 'R') ? 1 : -1;
    }

}
