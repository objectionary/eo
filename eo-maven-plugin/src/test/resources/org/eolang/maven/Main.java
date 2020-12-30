import org.eolang.*;
import org.eolang.phi.*;
public final class Main {
    public static void main(final String... args) throws Exception {
        final Phi[] list = new Phi[args.length];
        for (int idx = 0; idx < args.length; ++idx) {
            final Phi phi = new EOint();
            final long num = Long.parseLong(args[idx]);
            phi.attr("data").put(new Data.Value<>(num));
            list[idx] = phi;
        }
        final Phi app = new EOmain();
        app.attr("args").put(new Data.Value<>(list));
        if (!new Data.Take(app).take(Boolean.class)) {
            throw new RuntimeException();
        }
    }

}
