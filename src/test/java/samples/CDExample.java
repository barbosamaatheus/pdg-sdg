package samples;

import java.util.Arrays;
import java.util.List;

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
        String result = "";
        String allWords[];

        allWords = text.split(" ");
        for(int i = 0; i < allWords.length; i++) {
            for(int j = i+1; j < allWords.length; j++) {
                if(allWords[i].equals(allWords[j])) {
                    allWords[j] = "#remove#";
                }
            }
        }

        for(String word: allWords) {
            if(word != "#remove#") {
                result = result + word + " ";
            }
        }

        text = result;
    }
}