package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter
import java.util.*

/***
 * Represent an EO source code entity
 * (it may be anything from a source file to an abstraction to a free attribute)
 */
abstract class EOSourceEntity {
    /***
     * Transpiles the source entity to the target code model (it may be Java files, classes or class fragments)
     * @param w Java code generation utility to write the target code to
     *          (may be absent for higher-order source entities that need to transpile their children only)
     * @return A list of target Java files generated through translation
     *         (may be absent for source entities that map to file fragments, not separate Java files)
     */
    abstract fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>?
}