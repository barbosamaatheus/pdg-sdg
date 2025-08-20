package samples.PointerAnalysis;

public class Main {
    public static void main(String[] ars) {
        Report r = new ReportAdvanced();
        Text t = new Text(r);
        t.generateReport();
    }
}
