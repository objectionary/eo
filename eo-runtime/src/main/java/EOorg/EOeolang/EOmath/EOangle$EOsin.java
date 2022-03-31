package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sin.
 */
@XmirObject(oname = "angle.sin")
public class EOangle$EOsin extends PhDefault {

    public EOangle$EOsin(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(
            Math.sin(new Param(rho).strong(Double.class))
        )));
    }
}
