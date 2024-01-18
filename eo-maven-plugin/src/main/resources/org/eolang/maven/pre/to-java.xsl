<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="to-java" version="2.0">
  <xsl:import href="/org/eolang/parser/_datas.xsl"/>
  <xsl:param name="disclaimer"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="TAB">
    <xsl:text>  </xsl:text>
  </xsl:variable>
  <xsl:function name="eo:eol">
    <xsl:param name="tabs"/>
    <xsl:value-of select="'&#10;'"/>
    <xsl:value-of select="eo:tabs($tabs)"/>
  </xsl:function>
  <xsl:function name="eo:tabs">
    <xsl:param name="n"/>
    <xsl:for-each select="1 to $n">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:function>
  <xsl:function name="eo:type-of">
    <xsl:param name="root"/>
    <xsl:param name="o"/>
    <xsl:choose>
      <xsl:when test="$o/@base and $o/@ref">
        <xsl:copy-of select="eo:type-of($root, $root//o[@name=$o/@base and @line=$o/@ref])"/>
      </xsl:when>
      <xsl:when test="not($o/@base) and not($o/@ref) and contains($o/@line, '.')">
        <xsl:copy-of select="eo:type-of($root, $root//o[@line=substring-after($o/@line, '.') and @name=$o/@name])"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$o"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:clean" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO', replace(replace(replace(replace(replace($n, '_', '__'), '-', '_'), '@', 'φ'), 'α', '_'), '\$', '\$EO'))"/>
  </xsl:function>
  <xsl:function name="eo:suffix" as="xs:string">
    <xsl:param name="s1"/>
    <xsl:param name="s2"/>
    <xsl:value-of select="concat(concat($s1, '_'), $s2)"/>
  </xsl:function>
  <xsl:function name="eo:class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:param name="alt" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($n, '\.')"/>
    <xsl:variable name="p">
      <xsl:for-each select="$parts">
        <xsl:if test="position()!=last()">
          <xsl:value-of select="eo:clean(.)"/>
          <xsl:text>.</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:choose>
        <xsl:when test="$parts[last()]">
          <xsl:value-of select="$parts[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$parts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="pre" select="concat($p, eo:clean($c))"/>
    <xsl:choose>
      <xsl:when test="string-length($pre)&gt;250">
        <xsl:value-of select="concat(substring($pre, 1, 25), $alt)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$pre"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:attr-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$n='@'">
        <xsl:text>φ</xsl:text>
      </xsl:when>
      <xsl:when test="$n='^'">
        <xsl:text>ρ</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat('', $n)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="class/@name">
    <xsl:attribute name="name">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="java-name">
      <xsl:variable name="pkg" select="//metas/meta[head='package']/part[1]"/>
      <xsl:if test="$pkg">
        <xsl:value-of select="eo:class-name($pkg, eo:suffix(../@line, ../@pos))"/>
        <xsl:text>.</xsl:text>
      </xsl:if>
      <xsl:value-of select="eo:class-name(., eo:suffix(../@line, ../@pos))"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="class">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="java">
        <xsl:apply-templates select="/program" mode="license"/>
        <xsl:apply-templates select="//meta[head='package']" mode="head"/>
        <xsl:text>import org.eolang.*;</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:apply-templates select="//meta[head='junit' or head='tests']" mode="head"/>
        <xsl:apply-templates select="." mode="body"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="class" mode="body">
    <xsl:apply-templates select="xmir"/>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>@XmirObject(name = "</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>", oname = "</xsl:text>
    <xsl:choose>
      <xsl:when test="@original-name">
        <xsl:value-of select="@original-name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>", source = "</xsl:text>
    <xsl:value-of select="replace(/program/@source, '\\', '\\\\')"/>
    <xsl:text>")</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>public final class </xsl:text>
    <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
    <xsl:text> extends PhDefault {</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="." mode="ctors"/>
    <xsl:if test="//meta[head='junit' or head='tests'] and not(@parent)">
      <xsl:apply-templates select="." mode="tests"/>
    </xsl:if>
    <xsl:apply-templates select="class" mode="body"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="xmir">
    <xsl:for-each select="tokenize(text(), '&#10;')">
      <xsl:value-of select="eo:eol(0)"/>
      <xsl:text>// </xsl:text>
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="class" mode="ctors">
    <xsl:value-of select="eo:tabs(1)"/>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:choose>
      <xsl:when test="//meta[head='junit' or head='tests'] and not(@parent)">
        <xsl:text>public </xsl:text>
        <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
        <xsl:text>() {</xsl:text>
      </xsl:when>
      <xsl:when test="@ancestors">
        <xsl:text>public </xsl:text>
        <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
        <xsl:text>(final Phi sigma) {</xsl:text>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:text>super(sigma);</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>public </xsl:text>
        <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
        <xsl:text>(final Phi sigma) {</xsl:text>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:text>super(sigma);</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:variable name="type" select="concat(//meta[head='package']/tail, '.', @name)"/>
    <xsl:if test="$type='org.eolang.bytes'">
      <xsl:value-of select="eo:eol(2)"/>
      <xsl:text>this.add("Δ", new AtFree(new AtSimple()));</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="attr">
      <xsl:with-param name="class" select="."/>
      <xsl:with-param name="indent">
        <xsl:value-of select="eo:tabs(2)"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="attr">
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>this.add("</xsl:text>
    <xsl:value-of select="eo:attr-name(@name)"/>
    <xsl:text>", </xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>);</xsl:text>
  </xsl:template>
  <xsl:template match="free">
    <xsl:text>new AtFree(</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="bound">
    <xsl:text>new AtOnce(</xsl:text>
    <xsl:text>new AtComposite(this, rho -&gt; {</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="*">
      <xsl:with-param name="name" select="'ret'"/>
      <xsl:with-param name="indent">
        <xsl:value-of select="eo:tabs(3)"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:if test="o[@const]">
      <xsl:value-of select="eo:tabs(3)"/>
      <xsl:text>ret = new PhConst(ret);</xsl:text>
      <xsl:value-of select="eo:eol(0)"/>
    </xsl:if>
    <xsl:value-of select="eo:tabs(3)"/>
    <xsl:text>return ret;</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>})</xsl:text>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="o[not(@base) and @name]">
    <xsl:text>/</xsl:text>
    <xsl:text>* default */</xsl:text>
  </xsl:template>
  <xsl:template match="o[not(@base) and not(@name)]">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:text>new PhDefault() { </xsl:text>
    <xsl:text>/</xsl:text>
    <xsl:text>* anonymous abstract object without attributes */ };</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:function name="eo:fetch">
    <xsl:param name="object"/>
    <xsl:variable name="parts" select="tokenize($object, '\.')"/>
    <xsl:text>Phi.Φ</xsl:text>
    <xsl:for-each select="$parts">
      <xsl:text>.attr("</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>").get()</xsl:text>
    </xsl:for-each>
  </xsl:function>
  <xsl:template match="o[@base and not(starts-with(@base, '.'))]">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:variable name="current" select="."/>
    <xsl:variable name="source" select="//*[generate-id()!=generate-id($current) and @name=$current/@base and @line=$current/@ref]"/>
    <xsl:if test="count($source) &gt; 1">
      <xsl:message terminate="yes">
        <xsl:text>Found more than one target of '</xsl:text>
        <xsl:value-of select="$current/@base"/>
        <xsl:text>' at the line #</xsl:text>
        <xsl:value-of select="$current/@line"/>
        <xsl:text> leading to </xsl:text>
        <xsl:for-each select="$source">
          <xsl:if test="position()&gt;1">
            <xsl:text>, </xsl:text>
          </xsl:if>
          <xsl:text>&lt;</xsl:text>
          <xsl:value-of select="name(.)"/>
          <xsl:text>/&gt;</xsl:text>
          <xsl:text> at line #</xsl:text>
          <xsl:value-of select="@line"/>
        </xsl:for-each>
        <xsl:text>; it's an internal bug</xsl:text>
      </xsl:message>
    </xsl:if>
    <xsl:value-of select="$indent"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:choose>
      <xsl:when test="@primitive and @base">
        <xsl:text>new </xsl:text>
        <xsl:value-of select="eo:class-name(@base, eo:suffix(@line, @pos))"/>
        <xsl:text>(Phi.Φ)</xsl:text>
      </xsl:when>
      <xsl:when test="@base='$'">
        <xsl:text>rho</xsl:text>
      </xsl:when>
      <xsl:when test="@base='Q'">
        <xsl:text>Phi.Φ</xsl:text>
      </xsl:when>
      <xsl:when test="@base='^'">
        <xsl:text>new PhMethod(rho, "ρ")</xsl:text>
      </xsl:when>
      <xsl:when test="@base='&amp;'">
        <xsl:text>new PhMethod(rho, "σ")</xsl:text>
      </xsl:when>
      <xsl:when test="$source/@ancestors">
        <xsl:text>new </xsl:text>
        <xsl:value-of select="eo:class-name($source/@name, eo:suffix(@line, @pos))"/>
        <xsl:text>(rho)</xsl:text>
      </xsl:when>
      <xsl:when test="$source and name($source)='class'">
        <xsl:value-of select="eo:fetch(concat($source/@package, '.', $source/@name))"/>
      </xsl:when>
      <xsl:when test="$source/@level">
        <xsl:for-each select="0 to $source/@level">
          <xsl:text>new PhMethod(</xsl:text>
        </xsl:for-each>
        <xsl:text>rho</xsl:text>
        <xsl:for-each select="1 to $source/@level">
          <xsl:text>, "σ")</xsl:text>
        </xsl:for-each>
        <xsl:text>, "</xsl:text>
        <xsl:value-of select="$source/@name"/>
        <xsl:text>")</xsl:text>
      </xsl:when>
      <xsl:when test="$source">
        <xsl:text>new PhMethod(rho, "</xsl:text>
        <xsl:value-of select="eo:attr-name(@base)"/>
        <xsl:text>")</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="eo:fetch(@base)"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select=".[@copy]" mode="copy">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="located">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o[starts-with(@base, '.') and *]">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:apply-templates select="*[1]">
      <xsl:with-param name="name">
        <xsl:value-of select="$name"/>
        <xsl:text>_base</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="indent">
        <xsl:value-of select="$indent"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="$indent"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = new PhMethod(</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>_base, "</xsl:text>
    <xsl:variable name="method" select="substring-after(@base, '.')"/>
    <xsl:choose>
      <xsl:when test="$method='^'">
        <xsl:text>ρ</xsl:text>
      </xsl:when>
      <xsl:when test="$method='&amp;'">
        <xsl:text>σ</xsl:text>
      </xsl:when>
      <xsl:when test="$method='&lt;'">
        <xsl:text>ν</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="eo:attr-name($method)"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>");</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="." mode="located">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="skip" select="1"/>
    </xsl:apply-templates>
    <xsl:apply-templates select=".[@copy]" mode="copy">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o[@copy]" mode="copy">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:value-of select="$indent"/>
    <xsl:value-of select="eo:tabs(1)"/>
    <xsl:value-of select="$name"/>
    <xsl:text> = new PhCopy(</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>);</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="*" mode="located">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:if test="@line and @pos">
      <xsl:value-of select="$indent"/>
      <xsl:value-of select="eo:tabs(1)"/>
      <xsl:value-of select="$name"/>
      <xsl:text> = new PhLocated(</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="@line"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="@pos"/>
      <xsl:text>, </xsl:text>
      <xsl:text>"</xsl:text>
      <xsl:value-of select="@loc"/>
      <xsl:text>"</xsl:text>
      <xsl:text>);</xsl:text>
      <xsl:value-of select="eo:eol(0)"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="*" mode="application">
    <xsl:param name="indent"/>
    <xsl:param name="skip" select="0"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:param name="rho"/>
    <xsl:for-each select="./*[name()!='value' and name()!='tuple' and position() &gt; $skip][not(@level)]">
      <xsl:if test="position() = 1">
        <xsl:value-of select="$indent"/>
        <xsl:value-of select="$name"/>
        <xsl:text> = </xsl:text>
        <xsl:text>new PhCopy(</xsl:text>
        <xsl:value-of select="$name"/>
        <xsl:if test="$rho">
          <xsl:text>, </xsl:text>
          <xsl:value-of select="$rho"/>
        </xsl:if>
        <xsl:text>);</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
      </xsl:if>
      <xsl:variable name="n">
        <xsl:value-of select="$name"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="position()"/>
      </xsl:variable>
      <xsl:apply-templates select=".">
        <xsl:with-param name="name" select="$n"/>
        <xsl:with-param name="indent">
          <xsl:value-of select="$indent"/>
          <xsl:value-of select="eo:tabs(1)"/>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:for-each>
    <xsl:for-each select="./*[name()!='value' and name()!='tuple' and position() &gt; $skip][not(@level)]">
      <xsl:value-of select="$indent"/>
      <xsl:value-of select="eo:tabs(1)"/>
      <xsl:value-of select="$name"/>
      <xsl:text> = new PhWith(</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>, </xsl:text>
      <xsl:choose>
        <xsl:when test="@as">
          <xsl:choose>
            <xsl:when test="matches(@as,'^[0-9]+$')">
              <xsl:value-of select="eo:attr-name(@as)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>"</xsl:text>
              <xsl:value-of select="eo:attr-name(@as)"/>
              <xsl:text>"</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:when test="../@base = 'org.eolang.error'">
          <xsl:text>"α"</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="position() - 1"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>_</xsl:text>
      <xsl:value-of select="position()"/>
      <xsl:text>);</xsl:text>
      <xsl:value-of select="eo:eol(0)"/>
    </xsl:for-each>
    <xsl:apply-templates select="value|tuple">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent">
        <xsl:value-of select="$indent"/>
        <xsl:value-of select="eo:tabs(1)"/>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="value">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:value-of select="$indent"/>
    <xsl:value-of select="$name"/>
    <xsl:text> = new PhWith(</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>, "Δ", new Data.Value&lt;&gt;(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>));</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="class" mode="tests">
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>@Test</xsl:text>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>public void works() throws java.lang.Exception {</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:choose>
      <xsl:when test="starts-with(@name, 'throws')">
        <xsl:text>Assertions.assertThrows(Exception.class, () -&gt; {</xsl:text>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:apply-templates select="." mode="assert">
          <xsl:with-param name="indent" select="1"/>
        </xsl:apply-templates>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:text>});</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="assert">
          <xsl:with-param name="indent" select="0"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="class" mode="assert">
    <xsl:param name="indent"/>
    <xsl:value-of select="eo:tabs($indent)"/>
    <xsl:text>Object obj = new Dataized(new </xsl:text>
    <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
    <xsl:text>()).take(Boolean.class);</xsl:text>
    <xsl:value-of select="eo:eol(2 + $indent)"/>
    <xsl:text>if (obj instanceof String) {</xsl:text>
    <xsl:value-of select="eo:eol(2 + $indent)"/>
    <xsl:text>  Assertions.fail(obj.toString());</xsl:text>
    <xsl:value-of select="eo:eol(2 + $indent)"/>
    <xsl:text>} else {</xsl:text>
    <xsl:value-of select="eo:eol(2 + $indent)"/>
    <xsl:text>  Assertions.assertTrue((Boolean) obj);</xsl:text>
    <xsl:value-of select="eo:eol(2 + $indent)"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xsl:template match="meta[head='package']" mode="head">
    <xsl:text>package </xsl:text>
    <xsl:value-of select="eo:class-name(tail, '')"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="meta[head='junit' or head='tests']" mode="head">
    <xsl:text>import org.junit.jupiter.api.Assertions;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>import org.junit.jupiter.api.Test;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="/program" mode="license">
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>/* </xsl:text>
    <xsl:value-of select="$disclaimer"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
