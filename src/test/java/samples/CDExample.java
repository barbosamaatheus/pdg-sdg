package samples;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CDExample {
    public static String text;
    public static List<String> weaselWordsList = Arrays.asList("many", "various", "very", "fairly", "several", "extremely",
            "exceedingly", "quite", "remarkably", "few", "surprisingly", "mostly", "largely", "huge",
            "tiny", "is a number", "are a number", "excellent", "interestingly", "significantly",
            "substantially", "clearly", "vast", "relatively", "completely");

    public static void main(){
        if (text != null && hasWeaselWords()) { //LEFT
            removeWeaselWords();
            removeDuplicateWords(); //RIGHT
        }
    }

    public static boolean hasWeaselWords(){
        for (String word : text.split(" ")) {
            for (String s : weaselWordsList) {
                if (s.equals(word)) {
                    return true;
                }
            }
        }
        return false;
    }

    public static void removeWeaselWords(){
        for (String s : weaselWordsList) {
            text.replace(s, "");
        }
    }


    public static void removeDuplicateWords(){
        String[] words = text.split(" ");

        Set<String> set = new HashSet<>();
        for (String word : words) {
            set.add(word);
        }

        StringBuilder sb = new StringBuilder();
        for (String word : set) {
            sb.append(word).append(" ");
        }

    }
}