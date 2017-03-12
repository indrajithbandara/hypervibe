package stats;

/**
 * Created by adebesing on 12/03/2017.
 */
public class Core {
    public Core(){}

    public int[] irange(int from, int to) {
        int [] a = new int[1000000];
        for (int i = from; i < to; i++){
            a[i] = i;
        }
        return a;
    }
}
