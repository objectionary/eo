package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOfloat}
 */
public final class EOfloatEOcosTest {

    @Test
    public void cos0() {
        final double cos0 = Math.cos(0);
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(0)
                )
            ).take(Double.class),
            Matchers.equalTo(cos0)
        );
    }

    @Test
    public void cosPi() {
        final double cosPi = Math.cos(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(Math.PI)
                )
            ).take(Double.class),
            Matchers.equalTo(cosPi)
        );
    }

    @Test
    public void cosPiDiv2() {
        final double cosPiDiv2 = Math.cos(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(cosPiDiv2)
        );
    }

    @Test
    public void cosMinusPiDiv2() {
        final double cosMinusPiDiv2 = Math.cos(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(-Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(cosMinusPiDiv2)
        );
    }
}
