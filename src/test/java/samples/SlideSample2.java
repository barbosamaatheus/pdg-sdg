package samples;

public class SlideSample2 {
    public String text;

    public static void main(){
        SlideSample2 inst = new SlideSample2();

        inst.cleanText();
    }

    public void cleanText() {
        normalizeWhiteSpaces(); //Left
        removeComments();
        removeDuplicatedWords(); //Right
    }

    private void removeDuplicatedWords() {
        text = text;
    }

    private void removeComments() {
        text = text;
    }

    private void normalizeWhiteSpaces() {
        text = text;
    }
}
