package samples;

public class BlackBoard {
    public static int main(){
        ClassA a = new ClassA();
        a.x = 3;
        ClassA b = new ClassA();
        return a.x;
    }

    public static int m(int x){
        return x;
    }

}
