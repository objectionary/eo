package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOfloat}
 */
public final class EOfloatEOsinTest {

    @Test
    public void sin0() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOsin(
                    new Data.ToPhi(0)
                )
            ).take(Double.class),
            Matchers.equalTo(0)
        );
    }

    @Test
    public void sinPi() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOsin(
                    new Data.ToPhi(Math.PI)
                )
            ).take(Double.class),
            Matchers.equalTo(0)
        );
    }

    @Test
    public void sinPiDiv2() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOsin(
                    new Data.ToPhi(Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(1)
        );
    }

    @Test
    public void sinMinusPiDiv2() {
        MatcherAssert.assertThat(
            new Dataized(
                new EOfloat$EOsin(
                    new Data.ToPhi(-Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(-1)
        );
    }
}
