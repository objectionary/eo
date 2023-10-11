package EOorg.EOeolang.EOmath;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.junit.jupiter.api.Test;

public class EOnumberEOpowTest {
    @Test
    void pows() {
        Phi num = new PhWith(new EOnumber(Phi.Φ), "n", new Data.ToPhi(0L));
        new Dataized(
            new PhWith(new Data.ToPhi(0L).attr("eq").get().copy(), 0, num)
        ).take();
    }
}
