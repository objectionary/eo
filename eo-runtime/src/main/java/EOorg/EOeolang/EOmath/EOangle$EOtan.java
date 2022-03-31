package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Tangent.
 */
@XmirObject(oname = "angle.tan")
public class EOangle$EOtan extends PhDefault {

    public EOangle$EOtan(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.tan(new Param(rho).strong(Double.class))
        )));
    }
}
