package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Cos.
 */
@XmirObject(oname = "number.cos")
public class EOnumber$EOcos extends PhDefault {

    public EOnumber$EOcos(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.cos(new Param(rho.attr("ρ").get(), "n").strong(Double.class))
        )));
    }
}
