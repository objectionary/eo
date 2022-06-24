package EOorg.EOeolang.EOmath;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOangle$EOcos}
 */
public final class EOangleEOcosTest {

    @Test
    public void cos0() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(0.0d));
        final double cos0 = Math.cos(0.0d);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOcos(angle)).take(Double.class),
            Matchers.equalTo(cos0)
        );
    }

    @Test
    public void cosPi() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(Math.PI));
        final double cosPi = Math.cos(Math.PI);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOcos(angle)).take(Double.class),
            Matchers.equalTo(cosPi)
        );
    }

    @Test
    public void cosPiDiv2() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(Math.PI / 2));
        final double cosPiDiv2 = Math.cos(Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOcos(angle)).take(Double.class),
            Matchers.equalTo(cosPiDiv2)
        );
    }

    @Test
    public void cosMinusPiDiv2() {
        final Phi angle = new EOangle(Phi.Φ);
        angle.attr("f").put(new Data.ToPhi(-Math.PI / 2));
        final double cosMinusPiDiv2 = Math.cos(-Math.PI / 2);
        MatcherAssert.assertThat(
            new Dataized(new EOangle$EOcos(angle)).take(Double.class),
            Matchers.equalTo(cosMinusPiDiv2)
        );
    }
}
