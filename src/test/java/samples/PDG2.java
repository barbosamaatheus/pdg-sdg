package samples;

public class PDG2 {
    public static int main(){
        int sum, x;
        sum = 0;
        x = 1;
        while (x < 11) {
            sum = x + 1;
            x = sum + 1;
        }
        return x + sum;
    }
}
