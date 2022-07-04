package EOorg.EOeolang.EOmath;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOangle$EOsin}
 */
public final class EOangleEOsinTest {

    @Test
    public void sin0() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(0.0d));
        final double sin0 = Math.sin(0.0d);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOsin(angle)).take(Double.class),
            Matchers.equalTo(sin0)
        );
    }

    @Test
    public void sinPi() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(Math.PI));
        final double sinPi = Math.sin(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOsin(angle)).take(Double.class),
            Matchers.equalTo(sinPi)
        );
    }

    @Test
    public void sinPiDiv2() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(Math.PI / 2));
        final double sinPiDiv2 = Math.sin(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOsin(angle)).take(Double.class),
            Matchers.equalTo(sinPiDiv2)
        );
    }

    @Test
    public void sinMinusPiDiv2() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(-Math.PI / 2));
        final double sinMinusPiDiv2 = Math.sin(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOsin(angle)).take(Double.class),
            Matchers.equalTo(sinMinusPiDiv2)
        );
    }
}
