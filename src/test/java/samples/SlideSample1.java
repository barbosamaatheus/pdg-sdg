package samples;

public class SlideSample1 {
    public String text;
    int spaces;
    int comments;
    int words;

    public static void main(){
        SlideSample1 inst = new SlideSample1();

        inst.cleanText();
    }

    public int cleanText() {
        countDupWhiteSpace(); //Left
        countComments();
        countDupdWords(); //Right
        return spaces + words;
    }

    private void countDupWhiteSpace() {
        spaces = spaces + 1;
    }

    private void countDupdWords() {
        words = words + 1;
    }

    private void countComments() {
        comments = comments + 1;
    }
}
