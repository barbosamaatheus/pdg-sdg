package samples;

public class PDG1 {
    public static int main(){
        int sum, x;
        sum = 0;
        x = 1;
        while (x < 11) {
            sum = sum + 1;
            x = x + 1;
        }
        return x + sum;
    }
}
