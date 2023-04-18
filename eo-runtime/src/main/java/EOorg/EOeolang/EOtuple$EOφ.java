package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;

public class EOtuple$EOφ extends PhDefault {

    public EOtuple$EOφ(final Phi sigma) {
        super(sigma);
        this.add("φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi[] obj = new Phi[0];
                    return new Data.ToPhi(obj);
                }
            )
        );
        this.add("Δ", new AtComposite(
            this,
            rho -> {
                final Phi[] obj = new Phi[0];
                return new Data.ToPhi(obj);
            }
        ));
    }
}
