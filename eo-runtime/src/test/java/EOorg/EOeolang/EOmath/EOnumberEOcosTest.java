package EOorg.EOeolang.EOmath;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOnumber$EOcos}
 */
public final class EOnumberEOcosTest {

    @Test
    public void cos0() {
        final Phi number = new EOnumber(Phi.Φ);
        number.attr("n").put(new Data.ToPhi(0.0d));
        final double cos0 = Math.cos(0.0d);
        MatcherAssert.assertThat(
            new Dataized(new EOnumber$EOcos(number)).take(Double.class),
            Matchers.equalTo(cos0)
        );
    }

    @Test
    public void cosPi() {
        final Phi number = new EOnumber(Phi.Φ);
        number.attr("n").put(new Data.ToPhi(Math.PI));
        final double cosPi = Math.cos(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(new EOnumber$EOcos(number)).take(Double.class),
            Matchers.equalTo(cosPi)
        );
    }

    @Test
    public void cosPiDiv2() {
        final Phi number = new EOnumber(Phi.Φ);
        number.attr("n").put(new Data.ToPhi(Math.PI / 2));
        final double cosPiDiv2 = Math.cos(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOnumber$EOcos(number)).take(Double.class),
            Matchers.equalTo(cosPiDiv2)
        );
    }

    @Test
    public void cosMinusPiDiv2() {
        final Phi number = new EOnumber(Phi.Φ);
        number.attr("n").put(new Data.ToPhi(-Math.PI / 2));
        final double cosMinusPiDiv2 = Math.cos(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOnumber$EOcos(number)).take(Double.class),
            Matchers.equalTo(cosMinusPiDiv2)
        );
    }
}
