import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Possible algorithm solution of Advent of Code 2025 > Day 5 > Puzzle 2.
 * 
 * @author Pedro Reinaldo Mendes (https://pedrorm.com)
 * @version 1.0
 */
public class D5Puzzle2 {

    /**
     * Main function that prints out the solution for this puzzle.
     * 
     * @param args Command-line arguments (unused in this application).
     * @implSpec The input file must be well-formatted following the puzzle's
     *           instructions.
     * @implSpec The input file must contain at least one valid range.
     */
    public static void main(String[] args) throws IOException {

        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        List<long[]> ranges = new ArrayList<>();

        // We'll read the ranges from the first section of the file
        for (String line : lines) {

            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                break; // Stop reading after the first section as the rest is irrelevant
            }

            String[] parts = trimmed.split("-");
            ranges.add(new long[] { Long.parseLong(parts[0]), Long.parseLong(parts[1]) });
        }

        // Sort ranges by start value to facilitate merging
        ranges.sort(Comparator.comparingLong(a -> a[0]));

        long finalSolution = 0;

        List<long[]> mergedRanges = new ArrayList<>();
        mergedRanges.add(ranges.get(0));

        for (int i = 1; i < ranges.size(); i++) {
            long[] current = ranges.get(i);
            long[] last = mergedRanges.get(mergedRanges.size() - 1);

            // If current range overlaps with or is adjacent to the last range, merge them
            if (current[0] <= last[1] + 1) {
                last[1] = Math.max(last[1], current[1]);
            } else {
                mergedRanges.add(current);
            }
        }

        // Calculate the total count of fresh IDs from the merged distinct ranges
        for (long[] range : mergedRanges) {
            finalSolution += (range[1] - range[0] + 1);
        }

        System.out.println(finalSolution);
    }
}
