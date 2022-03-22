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
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(0)
                )
            ).take(Double.class),
            Matchers.equalTo(1)
        );
    }

    @Test
    public void cosPi() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(Math.PI)
                )
            ).take(Double.class),
            Matchers.equalTo(-1)
        );
    }

    @Test
    public void cosPiDiv2() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(0)
        );
    }

    @Test
    public void cosMinusPiDiv2() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOcos(
                    new Data.ToPhi(-Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(0)
        );
    }
}
