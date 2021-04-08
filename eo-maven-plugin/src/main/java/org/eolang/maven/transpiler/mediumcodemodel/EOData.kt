package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter
import org.eolang.EObool
import org.eolang.EOchar
import org.eolang.EOfloat
import org.eolang.EOint
import org.eolang.EOstring
import java.util.ArrayList

/***
 * Represents EO data type objects
 */
abstract class EOData : EOSourceEntity()

/***
 * Represents integer numerals
 */
class EOint(val value: Long) : EOData() {
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOint::class.java.simpleName}(${value}L)")
        return null
    }
}

/***
 * Represents floating-point numerals
 */
class EOfloat(val value: Double) : EOData() {
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOfloat::class.java.simpleName}(${value}D)")
        return null
    }
}

/***
 * Represents booleans
 */
class EObool(val value: Boolean) : EOData() {
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        w?.write("new ${EObool::class.java.simpleName}(${value})")
        return null
    }
}

/***
 * Represents strings
 */
class EOstring(val value: String) : EOData() {
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOstring::class.java.simpleName}(\"${value}\")")
        return null
    }
}

/***
 * Represents characters
 */
class EOchar(val value: Char) : EOData() {
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOchar::class.java.simpleName}('$value')")
        return null
    }
}