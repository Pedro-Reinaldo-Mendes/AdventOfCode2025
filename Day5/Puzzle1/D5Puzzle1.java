import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 5 > Puzzle 1.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.0
 * @implNote This is a lazy approach of checking each id with each interval. I
 *           thought about sorting the intervals (and perhaps even merging them)
 *           to then be able to compare each id only with the intervals starting
 *           on the interval whose first value corresponds to the nearest
 *           bottom-value of the id, and ending on the interval whose first
 *           value corresponds to the nearest top-value of the id.
 */
public class D5Puzzle1 {

    /**
     * Main function that prints out the solution for this puzzle.
     * 
     * @param args Command-line arguments (unused in this application).
     * @see #isFresh(int, List<long[]>)
     * @implSpec The input file must be well-formatted following the puzzle's
     *           instructions.
     */
    public static void main(String[] args) throws IOException {

        int finalSolution = 0;

        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        List<long[]> ranges = new ArrayList<>();
        List<Long> ingredientIds = new ArrayList<>();

        boolean readingRanges = true;

        // We'll need to go through each line in the input
        for (String line : lines) {
            if (line.trim().isEmpty()) {
                readingRanges = false;
                continue; // This represents the moment when we should simply go to the next line after
                          // finding the empty one and we know that from that moment on we'll just need to
                          // read ingredient IDs
            }

            // We must always act conforming if we're still reading the intervals or the
            // ingredient IDs already
            if (readingRanges) {
                String[] parts = line.split("-");
                ranges.add(new long[] { Long.parseLong(parts[0]), Long.parseLong(parts[1]) });
            } else {
                ingredientIds.add(Long.parseLong(line));
            }
        }

        // Now, for each id, we'll check if it's considered as fresh
        for (long id : ingredientIds) {
            if (isFresh(id, ranges)) {
                finalSolution++;
            }
        }

        System.out.println(finalSolution);
    }

    /**
     * This method checks if an ingredient ID is fresh by verifying if it falls
     * within any of the fresh ID ranges.
     * 
     * @param id     The ingredient ID to check.
     * @param ranges The list of fresh ingredient ID ranges.
     * @return True if the ingredient is fresh, false otherwise.
     * @throws NullPointerException if {@code ranges == null}
     */
    private static boolean isFresh(long id, List<long[]> ranges) {
        // We'll check if the id is inside any of the available intervals
        for (long[] range : ranges) {
            if (id >= range[0] && id <= range[1]) {
                return true;
            }
        }
        return false;
    }
}
