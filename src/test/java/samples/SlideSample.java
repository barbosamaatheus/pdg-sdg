package samples;

public class SlideSample {
    public String text;

    public static void main(){
        SlideSample inst = new SlideSample();

        if (inst.text != null && inst.hasWhiteSpace()){ //Left
            inst.normalizeWhiteSpaces();
            inst.removeDuplicatedWords(); //Right
        }
    }

    private void removeDuplicatedWords() {
        text = text;
    }

    private boolean hasWhiteSpace() {
        return text.contains(" ");
    }

    private void normalizeWhiteSpaces() {
        text = text;
    }
}
