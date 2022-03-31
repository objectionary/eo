package EOorg.EOeolang.EOmath;

import org.eolang.Data;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOangle}
 */
public final class EOangleEOtanTest {

    @Test
    public void tan0() {
        final double tan0 = Math.tan(0);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOtan(
                    new Data.ToPhi((double) 0)
                )
            ).take(Double.class),
            Matchers.equalTo(tan0)
        );
    }

    @Test
    public void tanPi() {
        final double tanPi = Math.tan(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOtan(
                    new Data.ToPhi(Math.PI)
                )
            ).take(Double.class),
            Matchers.equalTo(tanPi)
        );
    }

    @Test
    public void tanPiDiv2() {
        final double tanPiDiv2 = Math.tan(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOtan(
                    new Data.ToPhi(Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(tanPiDiv2)
        );
    }

    @Test
    public void tanMinusPiDiv2() {
        final double tanMinusPiDiv2 = Math.tan(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(
                new EOangle$EOtan(
                    new Data.ToPhi(-Math.PI / 2)
                )
            ).take(Double.class),
            Matchers.equalTo(tanMinusPiDiv2)
        );
    }
}
