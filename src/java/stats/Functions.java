package stats;

/**
 * Created by adebesing on 11/03/2017.
 */
public class Functions {

    public Functions(){}

    public double mean (double[] data) {
        double m = 0;
        double n = data.length;
        for (double d : data) {
            m += d;
        }
        return m / n;
    }
}
