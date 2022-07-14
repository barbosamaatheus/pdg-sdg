package samples;

public class SlideSample {
    public String text;

    public static void main(){
        SlideSample inst = new SlideSample();

        inst.cleanText();
    }

    public void cleanText() {
        if (text != null && hasWhiteSpace()){ //Left
            normalizeWhiteSpaces();
            removeDuplicatedWords(); //Right
        }
    }

    private void removeDuplicatedWords() {
        text = text.replace("aa", "");
    }

    private boolean hasWhiteSpace() {
        return text.contains(" ");
    }

    private void normalizeWhiteSpaces() {
        text = text.replace("  ", " ");
    }
}
