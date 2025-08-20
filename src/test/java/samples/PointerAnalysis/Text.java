package samples.PointerAnalysis;

public class Text {
    private Report r;
    Text(Report r) {
        this.r = r;
    }
    void generateReport() {
        r = new ReportSimple();
        //Text t = new Text(report);
        r.countDupWords(); // LEFT
        r.countComments();
        r.countDupWhiteSpace(); // RIGHT
    }
}
