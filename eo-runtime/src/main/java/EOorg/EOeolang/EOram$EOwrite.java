package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

@XmirObject(oname = "ram.write")
public class EOram$EOwrite extends PhDefault {
    public EOram$EOwrite(Phi sigma) {
        super(sigma);
        this.add("p", new AtFree());
        this.add("b", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final int pos = new Param(rho, "p").strong(Long.class).intValue();
            final byte[] bytes = new Param(rho, "b").strong(byte[].class);
            Ram.INSTANCE.write(rho.attr("ρ").get(), pos, bytes);
            return new Data.ToPhi(true);
        }));
    }
}
