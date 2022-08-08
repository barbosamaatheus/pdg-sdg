package samples;

public class SlideSample {
    public String text = "test123";

    public static void main(){
        SlideSample inst = new SlideSample();

        inst.cleanText();
    }

    public void cleanText() {
        if (text != null && hasWhiteSpace()){ //left
            normalizeWhiteSpaces();
            removeDuplicatedWords(); //right
        }
    }

    private void removeDuplicatedWords() {
        text = text + "AA";
    }

    private boolean hasWhiteSpace() {
        return true;
    }

    private void normalizeWhiteSpaces() {
        text = text + "  ";
    }
}