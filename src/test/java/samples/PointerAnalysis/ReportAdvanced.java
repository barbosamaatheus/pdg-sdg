package samples.PointerAnalysis;

public class ReportAdvanced implements Report {
    int fixes = 0;

    @Override
    public void countDupWords() {
        fixes++;
    }

    @Override
    public void countDupWhiteSpace() {
        int count = fixes;
        //fixes = count;
    }

    @Override
    public void countComments() {

    }
}
