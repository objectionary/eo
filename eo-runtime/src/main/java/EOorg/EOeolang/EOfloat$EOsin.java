package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sin.
 */
@XmirObject(oname = "float.sin")
public class EOfloat$EOsin extends PhDefault {

    public EOfloat$EOsin(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.sin(new Param(rho).strong(Double.class))
        )));
    }
}
