package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * COS.
 */
@XmirObject(oname = "cos")
public class EOcos extends PhDefault {

    public EOcos(final Phi sigma) {
        super(sigma);
        this.add("x", new AtFree());
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.cos(new Param(rho, "x").strong(Double.class))
        )));
    }
}