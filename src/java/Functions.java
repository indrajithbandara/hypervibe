package java;

/**
 * Created by adebesing on 11/03/2017.
 */
public class Functions {

    public Integer mean (java.util.List<Integer> data) {
        int m = 0;
        int n = data.size();
        for (Integer d : data) {
            m += d;
        }
        return m / n;
    }
}
