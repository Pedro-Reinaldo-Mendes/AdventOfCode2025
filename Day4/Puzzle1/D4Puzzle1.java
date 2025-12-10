import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.io.BufferedReader;
import java.io.IOException;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 4 > Puzzle 1.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.0
 * @implNote This is a brute-lazy approach that we'll be using for now.
 */
public class D4Puzzle1 {

    /**
     * Main function that prints out the solution for this puzzle.
     * 
     * @param args Command-line arguments (unused in this application).
     * @see #accessibleRolls(char[][])
     * @implSpec The input file must be well-formatted following the puzzle's
     *           instructions.
     */
    public static void main(String[] args) {

        try (BufferedReader br = Files.newBufferedReader(Paths.get("input.txt"))) {

            // We first must know how many lines is the input long and put it as number of
            // rows in our helper map
            char[][] map = new char[Files.readAllLines(Paths.get("input.txt")).size()][];

            // Now, we can put all the characters with boolean representations in our matrix
            String line;

            for (int i = 0; (line = br.readLine()) != null; i++) {
                map[i] = line.toCharArray();
            }

            int solution = accessibleRolls(map);
            System.out.println(solution);

        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }

    /**
     * This function returns the number of rolls of paper with fewer than four rolls
     * in the eight adjacent positions.
     * 
     * @param map The character's map with '@' indicating a roll of paper and '.'
     *            void.
     * @return The number of rolls of paper with fewer than four rolls in the eight
     *         adjacent positions.
     * @throws NullPointerException if {@code map == null}
     * @implSpec The input map must be well-formatted as indicated in the puzzle's
     *           instructions.
     */
    private static int accessibleRolls(char[][] map) {

        int accessibleRolls = 0;
        int adjRolls;

        // We'll go through each character and check how many rolls are in adjacent
        // positions
        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[i].length; j++) {

                // We also need to check if the initial point is a roll of paper or void
                if (map[i][j] == '@') {

                    adjRolls = 0;

                    // For each roll of paper, we'll go through it's adjacent positions
                    for (int i2 = -1; i2 <= 1; i2++) {
                        for (int j2 = -1; j2 <= 1; j2++) {

                            if ((i2 != 0 || j2 != 0) && i + i2 > -1 && i + i2 < map.length && j + j2 > -1
                                    && j + j2 < map[i].length && map[i + i2][j + j2] == '@')
                                adjRolls++;

                        }
                    }

                    if (adjRolls < 4)
                        accessibleRolls++;
                }

            }
        }

        // We finally return the total accessible rolls of paper
        return accessibleRolls;
    }

}