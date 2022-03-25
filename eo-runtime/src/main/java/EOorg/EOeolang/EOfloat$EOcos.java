package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Cos.
 */
@XmirObject(oname = "float.cos")
public class EOfloat$EOcos extends PhDefault {

    public EOfloat$EOcos(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.cos(new Param(rho).strong(Double.class))
        )));
    }
}
