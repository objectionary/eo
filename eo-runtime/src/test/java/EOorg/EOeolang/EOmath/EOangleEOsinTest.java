package EOorg.EOeolang.EOmath;

import EOorg.EOeolang.EOmath.EOangle$EOsin;
import org.eolang.Data;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOangle}
 */
public final class EOangleEOsinTest {

    @Test
    public void sin0() {
        final double sin0 = Math.sin(0);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOsin(
                    new Data.ToPhi((double) 0)
                )
            ).take(Double.class),
            Matchers.equalTo(sin0)
        );
    }

    @Test
    public void sinPi() {
        final double sinPi = Math.sin(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOsin(
                    new Data.ToPhi(Math.PI)
                )
            ).take(Double.class),
            Matchers.equalTo(sinPi)
        );
    }

    @Test
    public void sinPiDiv2() {
        final double sinPiDiv2 = Math.sin(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOsin(
                    new Data.ToPhi(Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(sinPiDiv2)
        );
    }

    @Test
    public void sinMinusPiDiv2() {
        final double sinMinusPiDiv2 = Math.sin(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOsin(
                    new Data.ToPhi(-Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(sinMinusPiDiv2)
        );
    }
}
