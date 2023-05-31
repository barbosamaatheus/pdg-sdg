package samples;

import java.util.Arrays;
import java.util.List;

public class CDExample {
    public String text;
    public List<String> weaselWordsList = Arrays.asList("many", "various", "very", "fairly", "several", "extremely",
            "exceedingly", "quite", "remarkably", "few", "surprisingly", "mostly", "largely", "huge",
            "tiny", "is a number", "are a number", "excellent", "interestingly", "significantly",
            "substantially", "clearly", "vast", "relatively", "completely");

    public static void main(){
        CDExample inst = new CDExample();
        inst.cleanText();
    }

    public void cleanText(){
        if (text != null && hasWeaselWords()) { //LEFT
            removeWeaselWords();
            removeDuplicateWords(); //RIGHT
        }
    }

    public boolean hasWeaselWords(){
        for (String word : text.split(" ")) {
            for (String s : weaselWordsList) {
                if (s.equals(word)) {
                    return true;
                }
            }
        }
        return false;
    }

    public void removeWeaselWords(){
        for (String s : weaselWordsList) {
            text.replace(s, "");
        }
    }

    public void removeDuplicateWords(){
        String[] words = text.split(" ");
        StringBuilder result = new StringBuilder(words[0]);
        for (int i = 1; i < words.length; i++) {
            if (!words[i].equals(words[i - 1])) {
                result.append(" ");
                result.append(words[i]);
            }
        }

        text = result.toString();
    }
}