<!-- Hey, emacs, please consider this to be -*- xml -*-

    This is an alternative to html.xsl for single-page output.
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://lichteblau.com/macro"
                version="1.0">

  <xsl:import href="html-common.tmp"/>
  <xsl:include href="base-uri.xsl"/>
  <xsl:output method="xml" indent="yes"/>

  <xsl:key name="about-symbol"      match="about-symbol"      use="string(.)"/>
  <xsl:key name="about-variable"    match="about-variable"    use="string(.)"/>
  <xsl:key name="about-function"    match="about-function"    use="string(.)"/>
  <xsl:key name="about-generic"     match="about-generic"     use="string(.)"/>
  <xsl:key name="about-operator"    match="about-operator"    use="string(.)"/>
  <xsl:key name="about-macro"       match="about-macro"       use="string(.)"/>
  <xsl:key name="about-type"        match="about-type"        use="string(.)"/>
  <xsl:key name="about-class"       match="about-class"       use="string(.)"/>
  <xsl:key name="about-systemclass" match="about-systemclass" use="string(.)"/>
  <xsl:key name="about-struct"      match="about-struct"      use="string(.)"/>
  <xsl:key name="about-condition"   match="about-condition"   use="string(.)"/>

  <xsl:key name="id"
           match="symbol-definition|
                  variable-definition|
                  function-definition|
                  generic-definition|
                  operator-definition|
                  macro-definition|
                  type-definition|
                  class-definition|
                  systemclass-definition|
                  struct-definition|
                  condition-definition"
           use="@id"/>

  <xsl:key name="symbol-by-name"
           match="symbol-definition"
           use="@name"/>
  <xsl:key name="variable-by-name"
           match="variable-definition"
           use="@name"/>
  <xsl:key name="function-by-name"
           match="function-definition"
           use="@name"/>
  <xsl:key name="generic-by-name"
           match="generic-definition"
           use="@name"/>
  <xsl:key name="operator-by-name"
           match="operator-definition"
           use="@name"/>
  <xsl:key name="macro-by-name"
           match="macro-definition"
           use="@name"/>
  <xsl:key name="type-by-name"
           match="type-definition"
           use="@name"/>
  <xsl:key name="class-by-name"
           match="class-definition|type-definition"
           use="@name"/>
  <xsl:key name="systemclass-by-name"
           match="systemclass-definition"
           use="@name"/>
  <xsl:key name="struct-by-name"
           match="struct-definition"
           use="@name"/>
  <xsl:key name="condition-by-name"
           match="condition-definition"
           use="@name"/>

  <xsl:template match="/">
    <pages>
      <xsl:call-template name="copy-base-uri"/>
      <macro:copy-attribute name="logo" path="documentation"/>
      <macro:copy-attribute name="css" path="documentation"/>
      <macro:copy-attribute name="heading" path="documentation"/>
      <macro:copy-attribute name="ico" path="documentation"/>
      <xsl:apply-templates select="documentation"/>
    </pages>
  </xsl:template>

  <xsl:template match="documentation">
    <main-page title="{@index-title}"
               author="{/documentation/@author}"
               author-url="{/documentation/@author-url}"
               date="{/documentation/@date}"
               keywords="Lisp, Documentation, Package, {@name}"
               single-page="yes">
      <div id="sp-about-packages">
        <xsl:for-each select="package">
          <p>
            <i>About <xsl:value-of select="@name"/>:</i>
            <xsl:apply-templates select="documentation-string"/>
          </p>
        </xsl:for-each>
      </div>
      <xsl:if test="package/sections">
        <h3>Contents</h3>
        <div class="indent">
          <ul>
            <xsl:for-each select="package">
            <li>
              Package <xsl:value-of select="@name"/>
              <ul>
                <xsl:for-each select="sections/section">
                  <li>
                    <a href="#{generate-id()}">
                      <xsl:value-of select="@section"/>
                    </a>
                  </li>
                  </xsl:for-each>
                </ul>
              </li>
            </xsl:for-each>
          </ul>
        </div>
      </xsl:if>
      <xsl:apply-templates select="package"/>
      <h3><a name="index"></a>Index of Exported Symbols</h3>
      <simple-table>
        <xsl:apply-templates select="package/external-symbols/*"
                             mode="symbol-index">
          <xsl:sort select="@name" data-type="text" order="ascending"/>
          <xsl:with-param name="packagep" select="'pages/'"/>
        </xsl:apply-templates>
      </simple-table>
    </main-page>
  </xsl:template>

  <!--
      Other templates
    -->

  <xsl:template name="main-documentation-string">
    <xsl:if test="see-also/constructor">
      <div class="sph3">Returned by:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/constructor/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <div class="sph3">Slot Access Functions:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/slot/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:variable name="inherited-slots"
                  select="key('id', current()//superclass/@id)//see-also/slot"/>
    <xsl:if test="$inherited-slots">
      <div class="sph3">Inherited Slot Access Functions:</div>
      <div>
        <ul>
         <xsl:apply-templates select="$inherited-slots/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="documentation-string">
        <div class="sph3">Details:</div>
        <xsl:apply-templates select="documentation-string"/>
      </xsl:when>
      <xsl:otherwise>
        <p style="color: red; font-weight: bold">
          No documentation string.  Possibly unimplemented or incomplete.
        </p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
    <xsl:apply-templates select="note"/>
    <xsl:if test="see-also/condition">
      <div class="sph3">Condition Types Signalled:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/condition/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <div class="sph3">See also:</div>
      <div class="indent">
        <simple-table>
          <xsl:for-each select="see-also/other/see|see-also/auto/see">
          <xsl:variable name="name" select="text()"/>
          <xsl:if test="not(preceding-sibling::see[text() = $name])">
            <a href="#{@id}">
              <span class="sym">
                <xsl:apply-templates/>
              </span>
            </a>
            <br/>
          </xsl:if> 
        </xsl:for-each>
      </simple-table>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template name="main">
    <macro:maybe-columns test="see-also">
      <xsl:call-template name="main-left"/>
    </macro:maybe-columns>
  </xsl:template>

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
    <a href="#{@id}">
      <tt>
        <xsl:if test="@status = 'INTERNAL'">
          <xsl:value-of select="@package"/>
          <xsl:text>::</xsl:text>
        </xsl:if>
        <xsl:value-of select="@name"/>
      </tt>
    </a>
      </xsl:when>
      <xsl:when test="@status = 'INTERNAL'">
    <tt style="color: #777777">
      <xsl:value-of select="@package"/>
      <xsl:text>::</xsl:text>
      <xsl:value-of select="@name"/>
    </tt>
      </xsl:when>
      <xsl:otherwise>
    <tt style="color: #777777">
      <xsl:value-of select="@package"/>
      <xsl:text>:</xsl:text>
      <xsl:value-of select="@name"/>
    </tt>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="lambda-list">
    <tt><xsl:value-of select="../@name"/></tt>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
    <xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

<!--
  <xsl:template name="slot-list">
    <div class="indent">
      <simple-table>
        <xsl:value-of select="@name"/>
    <xsl:text> .. </xsl:text>
        <xsl:choose>
          <xsl:when
            test="documentation-string/short">
            <xsl:apply-templates select="documentation-string/short"/>
        <a href="#{@id}">...</a>
      </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates
              select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      </simple-table>
    </div>
  </xsl:template>
-->

  <xsl:template name="slot-list">
    <div class="indent">
      <simple-table>
        <span class="code">
          <xsl:value-of select="@name"/>
        </span>
        <xsl:text> -- </xsl:text>
        <xsl:apply-templates select="documentation-string"/>
<!--
        <xsl:choose>
          <xsl:when test="documentation-string/short">
            <xsl:apply-templates select="documentation-string/short"/>
            <a href="{@id}.html#details">...</a>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates
              select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
-->
      </simple-table>
    </div>
  </xsl:template>

  <xsl:template name="definition">
    <xsl:param name="label"/>
    <a name="{@id}"/>
    <div class="sp-lambda-list">
      <b>
    <xsl:value-of select="$label"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@name"/>
      </b>
      <xsl:if test="lambda-list">
    <xsl:text> (</xsl:text>
    <xsl:for-each select="lambda-list/elt">
      <xsl:if test="position() != 1">
        <xsl:text>&#160;</xsl:text>
      </xsl:if>
      <xsl:value-of select="text()"/>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>



  <xsl:template match="a">
    <a href="{@a}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="slot">
    <a href="#{@id}">
      <tt>
    <xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>


  <xsl:template match="method">
    <span class="sym">
      <xsl:value-of select="@method"/>
    </span>
    <span class ="code">
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="see">
    <li>
      <a href="#{@id}">
    <tt>
      <xsl:apply-templates/>
    </tt>
      </a>
      <xsl:if test="@see">
    &#160;
    <i>
      <xsl:value-of select="@see"/>
    </i>
      </xsl:if>
    </li>
  </xsl:template>

  <!-- Dictionary Entries -->

  <xsl:template match="dictionary">
    <div class="noindent">
      <div class="sph3"><xsl:value-of select="@dictionary"/>:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="affected-by">
    <div class="noindent">
      <div class="sph3">Affected By:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="examples">
    <div class="noindent">
      <div class="sph3">Examples:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="exceptional">
    <div class="noindent">
      <div class="sph3">Exceptional Situations:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="notes">
    <div class="noindent">
      <div class="sph3">Notes:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="methods">
    <div class="noindent">
      <div class="sph3">Methods:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-kind">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Kind:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-syntax">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Syntax:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-arguments">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Arguments:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-description">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Description:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="supertypes">
    <div class="sph3">Supertypes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="valid-context">
    <div class="sph3">Valid Context:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="binding-types">
    <div class="sph3">Binding Types Affected:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="syntax">
    <div class="sph3">Syntax:</div>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="syntax">
          <xsl:choose>
            <xsl:when test="not(@syntax='')">
              <span class="code">
                <xsl:value-of select="@syntax"/>
                <xsl:text> ::= </xsl:text>
                <xsl:apply-templates/>
              </span>
            </xsl:when>
            <xsl:otherwise>
              <span class="code">
                <xsl:apply-templates/>
              </span>
            </xsl:otherwise>
          </xsl:choose>
          <br/>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>

  <!-- End of Dictionary Entries -->

  <xsl:template match="implementation-note">
    <div class="sph3">Implementation notes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="note">
    <div class="sph3">Notes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-type">
    <div class="sph3">Value Type:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-value">
    <div class="sph3">Initial Value:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="heading">
    <div class="noindent">
      <div class="sph3">
        <xsl:apply-templates/>:
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subheading">
    <div class="sph4">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="section">
    <h3>
      <a name="{generate-id()}"/>
      <xsl:value-of select="@section"/>
    </h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

<!-- **********************************************************************  -->

  <xsl:template match="about-variable">
    <xsl:apply-templates select="key('variable-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-function">
    <xsl:apply-templates select="key('function-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-generic">
    <xsl:apply-templates select="key('generic-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-operator">
    <xsl:apply-templates select="key('operator-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-macro">
    <xsl:apply-templates select="key('macro-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-type">
    <xsl:apply-templates select="key('type-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-symbol">
    <xsl:apply-templates select="key('symbol-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-class">
    <xsl:apply-templates select="key('class-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-systemclass">
    <xsl:apply-templates select="key('systemclass-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-struct">
    <xsl:apply-templates select="key('struct-by-name', current())"/>
  </xsl:template>

  <xsl:template match="about-condition">
    <xsl:apply-templates select="key('condition-by-name', current())"/>
  </xsl:template>

<!-- **********************************************************************  -->

  <xsl:template match="arguments">
    <div class="sph3">Arguments:</div>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="argument">
          <xsl:choose>
            <xsl:when test="not(@argument='')">
              <span class="arg">
                <xsl:value-of select="@argument"/>
              </span>
              <xsl:text> -- </xsl:text>
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates/>
            </xsl:otherwise>
          </xsl:choose>
          <br/>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>

<!--
  <xsl:template match="arguments">
    <div class="sph3">Arguments:</div>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="argument">
          <span class="arg">
            <xsl:value-of select="@argument"/>
          </span>
          <xsl:text>  </xsl:text>
          <xsl:apply-templates/>
          <br/>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>
-->

  <xsl:template match="variable-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Variable'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="variable-type"/>
        <xsl:apply-templates select="variable-value"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="function-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Function'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="generic-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Generic Function'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="operator-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Special Operator'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="macro-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Macro'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="type-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Type'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="supertypes"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="symbol-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Symbol'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:apply-templates select="valid-context"/>
        <xsl:apply-templates select="binding-types"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>
          <div class="indent">
            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="systemclass-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'System Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>
          <div class="indent">
            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="struct-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Struct'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>
          <div class="indent">
            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="condition-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Condition Type'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>
          <div class="indent">
            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
        (undocumented)
      </span>
    </xsl:if>
  </xsl:template>

  <xsl:template match="package">
    <xsl:apply-templates select="sections/section"/>
    <xsl:variable name="unreferenced"
                  select="external-symbols/function-definition[
                          count(key('about-function',@name))=0]"/>
    <xsl:if test="$unreferenced">
      <h3>Other Functions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced5"
                  select="external-symbols/operator-definition[
                          count(key('about-operator',@name))=0]"/>
    <xsl:if test="$unreferenced5">
      <h3>Other Special Operators in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced5">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced6"
                  select="external-symbols/symbol-definition[
                          count(key('about-symbol',@name))=0]"/>
    <xsl:if test="$unreferenced6">
      <h3>Other Symbols in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced6">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced7"
                  select="external-symbols/condition-definition[
                  count(key('about-condition',@name))=0]"/>
    <xsl:if test="$unreferenced7">
      <h3>Other Conditions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced7">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced2"
                  select="external-symbols/macro-definition[
                          count(key('about-macro',@name))=0]"/>
    <xsl:if test="$unreferenced2">
      <h3>Other Macros in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced2">
    <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced3"
                  select="external-symbols/class-definition[
                          count(key('about-class',@name))=0]"/>
    <xsl:if test="$unreferenced3">
      <h3>Other Classes in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced3">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

    <xsl:variable name="unreferenced4"
                  select="external-symbols/variable-definition[
                          count(key('about-variable',@name))=0]"/>
    <xsl:if test="$unreferenced4">
      <h3>Other Variables in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced4">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>

  </xsl:template>
</xsl:stylesheet>
