<!-- Hey, emacs, please consider this to be -*- xml -*-
    This is the main stylesheet.
    Input must have been cleaned up using cleanup.xsl already.

    This stylesheet does nearly all of the formatting work, but still keeps
    all data together in one big XML document.

    A <page> element is produced for each package and symbol.

    The contents of each <page> will be mostly HTML, with the exception
    of a few formatting elements like <columns> that are replaced later.

  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://lichteblau.com/macro"
                version="1.0">

  <xsl:import href="html-common.tmp"/>
  <xsl:include href="base-uri.xsl"/>
  <xsl:output method="xml" indent="yes"/>

  <xsl:key name="id"
           match="variable-definition |
                  function-definition |
                  generic-definition |
                  operator-definition |
                  macro-definition |
                  type-definition |
                  symbol-definition |
                  class-definition |
                  systemclass-definition |
                  struct-definition |
                  condition-definition"
           use="@id"/>

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
  <xsl:key name="symbol-by-name"
           match="symbol-definition"
           use="@name"/>
  <xsl:key name="class-by-name"
           match="class-definition"
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
      <xsl:call-template name="configuration-attributes"/>
      <xsl:apply-templates select="documentation"/>
      <xsl:apply-templates select="documentation/package"/>
      <xsl:apply-templates select="documentation/package/external-symbols/*"/>
      <xsl:if test="documentation/@include-internal-symbols-p">
        <xsl:apply-templates select="documentation/package/internal-symbols/*"/>
      </xsl:if>
      <!-- <xsl:apply-templates select="documentation/package/*/class-definition/direct-slots/*"/> -->
    </pages>
  </xsl:template>

  <xsl:template name="configuration-attributes">
    <macro:copy-attribute name="logo" path="documentation"/>
    <macro:copy-attribute name="css" path="documentation"/>
    <macro:copy-attribute name="heading" path="documentation"/>
    <macro:copy-attribute name="ico" path="documentation"/>
  </xsl:template>

  <!--
      page generation templates
    -->

  <xsl:template match="documentation">
    <main-page title="{@index-title}"
               author="{/documentation/@author}"
               author-url="{/documentation/@author-url}"
               date="{/documentation/@date}"
               keywords="Lisp, Documentation, Package, {@name}">
      <padded>
        Index of packages:
      </padded>
      <columns>
        <column width="60%">
          <padded>
            <xsl:for-each select="package">
              <xsl:variable name="url"
                            select="concat('pages/', @id, '.html')"/>
              <h2 class="page-title">
                <a href="{$url}">
                  Package
                  <xsl:value-of select="@name"/>
                </a>
              </h2>
              <div style="left: 100px">
                <xsl:apply-templates select="documentation-string"/>
                <div class="indent">
                  <xsl:if test="sections">
                    <p><i>About this package:</i></p>
                    <ul>
                      <xsl:for-each select="sections/section">
                        <li>
                          <a href="{$url}#{generate-id()}">
                            <xsl:value-of select="@section"/>
                          </a>
                        </li>
                      </xsl:for-each>
                    </ul>
                  </xsl:if>
                </div>
              </div>
            </xsl:for-each>
          </padded>
        </column>
        <column>
          <h3><a name="index"></a>Exported Symbol Index</h3>
          <simple-table>
            <xsl:apply-templates select="package/external-symbols/*"
                                 mode="symbol-index">
              <xsl:sort select="@name" data-type="text" order="ascending"/>
              <xsl:with-param name="packagep" select="'pages/'"/>
            </xsl:apply-templates>
          </simple-table>
        </column>
      </columns>
    </main-page>
  </xsl:template>

  <xsl:template match="package">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Package {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Package, {@name}">
      <padded>
    <xsl:if test="count(../package) > 1">
      <p class="noindent">
        Up:
        <a href="../index.html">
          <xsl:value-of select="/documentation/@index-title"/>
        </a>
      </p>
    </xsl:if>
    <h1>
      Package
      <xsl:value-of select="@name"/>
    </h1>
    <xsl:apply-templates select="documentation-string"/>
      </padded>
      <columns>
    <column width="60%">
      <padded>
        <xsl:if test="sections">
          <div style="margin-left: -30px">
        <h3>About This Package</h3>
          </div>
          <xsl:for-each select="sections/section">
        <a href="#{generate-id()}" style="font-weight: bold">
          <xsl:value-of select="@section"/>
        </a>
        <br/>
          </xsl:for-each>
          <br/>
          <xsl:apply-templates select="sections"/>
        </xsl:if>
      </padded>
    </column>
    <column>
      <h3><a name="index"></a>Exported Symbol Index</h3>
      <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
    </column>
      </columns>
    </page>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:for-each select="section">
      <h2>
        <a name="{generate-id()}"/>
        <xsl:value-of select="@section"/>
      </h2>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="section-page">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Section {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Package, {@name}">
      <padded>
        <xsl:if test="count(../package) > 1">
          <p class="noindent">
            Up:
            <a href="../index.html">
              <xsl:value-of select="/documentation/@index-title"/>
            </a>
          </p>
        </xsl:if>
        <h1>
          Package
          <xsl:value-of select="@name"/>
        </h1>
        <xsl:apply-templates select="documentation-string"/>
      </padded>
      <columns>
        <column width="60%">
          <padded>
            <xsl:if test="sections">
              <div style="margin-left: -30px">
                <h3>About This Package</h3>
              </div>
              <xsl:for-each select="sections/section">
                <a href="#{generate-id()}" style="font-weight: bold">
                  <xsl:value-of select="@section"/>
                </a>
                <br/>
              </xsl:for-each>
                <br/>
                <xsl:apply-templates/>
            </xsl:if>
          </padded>
        </column>
        <column>
          <h3><a name="index"></a>Exported Symbol Index</h3>
          <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
        </column>
      </columns>
    </page>
  </xsl:template>


  <xsl:template name="main-left">
    <xsl:choose>
      <xsl:when test="documentation-string">
        <h3>Details</h3>
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
  </xsl:template>

  <xsl:template name="main-right">
    <xsl:if test="see-also/constructor">
      <h3>Returned by</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/constructor/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/condition">
      <h3>Condition Types Signalled</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/condition/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <h3>Slot Access Functions</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/slot/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if
       test="key('id', .//superclass/@id)//see-also/slot">
      <h3>Inherited Slot Access Functions</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates
              select="key('id', .//superclass/@id)//see-also/slot/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <h3>See also</h3>
      <div class="indent">
        <simple-table>
          <xsl:for-each select="see-also/other/see|see-also/auto/see">
            <xsl:variable name="name" select="text()"/>
            <xsl:if test="not(preceding-sibling::see[text() = $name])">
              <xsl:apply-templates select="."/>
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

<!-- Pages for the definitions of the external symbols -->

  <xsl:template match="symbol-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Type {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Symbol, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="valid-context"/>
          <xsl:apply-templates select="binding-types"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="variable-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Variable {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Variable, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="variable-type"/>
          <xsl:apply-templates select="variable-value"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="function-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Function {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Function, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="generic-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Generic Function {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Generic Function, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="operator-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Special Operator {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Special Operator, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="macro-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Macro {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Macro, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
           <xsl:value-of select="../../@name"/>
         </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="type-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Type {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Type, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="supertypes"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Class {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Class, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
      Class <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
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
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="systemclass-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, System Class, {@name}"
          title="System Class {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          System Class <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns
       test="see-also or key('id', .//superclass/@id)//see-also/slot">
        <padded>
          <h3>Superclasses</h3>
          <div class="indent">
            <xsl:for-each select="cpl/superclass">
              <xsl:call-template name="class-list"/>
            </xsl:for-each>
          </div>
          <h3>Documented Subclasses</h3>
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
            <h3>Direct Slots</h3>
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
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="struct-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Struct {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Structure, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
      Struct <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
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
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="condition-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Condition {@name}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Condition Type, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
      Condition Type <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
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
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <!--
      Other templates
    -->

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
    <span class="sym">
      <xsl:value-of select="../@name"/>
    </span>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
        <xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

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

  <xsl:template name="about-arguments">
    <div class="def">
      <a href="{../@id}.html">
        <xsl:value-of select="../@kind-name"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="../@name"/>
        <xsl:text> (</xsl:text>
        <xsl:for-each select="elt">
          <xsl:if test="position() != 1">
            <xsl:text>&#160;</xsl:text>
          </xsl:if>
          <xsl:value-of select="text()"/>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </a>
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

  <xsl:template match="var">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="fun">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="gen">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="macro">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="type">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="symbol">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="operator">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="class">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="systemclass">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="struct">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="condition">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="slot">
    <a href="{@id}.html">
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
    <tr>
      <td>
        <a href="{@id}.html">
          <span class="code">
            <xsl:apply-templates/>
          </span>
        </a>
      </td>
      <xsl:if test="@see">
        <td>
          &#160;&#160;&#160;&#160;
          <i>
            <xsl:value-of select="@see"/>
          </i>
        </td>
      </xsl:if>
    </tr>
  </xsl:template>

  <xsl:template match="return">
    <h3>Return Value</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="heading">
    <div class="noindent">
    <h3><xsl:apply-templates/></h3>
    </div>
  </xsl:template>

  <xsl:template match="subheading">
    <h4><xsl:apply-templates/></h4>
  </xsl:template>

  <xsl:template match="implementation-note">
    <h3>Implementation notes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="note">
    <h3>Notes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <!-- Dictionary Entries -->

  <xsl:template match="dictionary">
    <div class="noindent">
      <h3>
        <xsl:value-of select="@dictionary"/>
      </h3>
    </div>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="variable-type">
    <h3>Value Type</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-value">
    <h3>Initial Value</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="see-also">
    <div class="noindent">
      <h3>See Also</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="supertypes">
    <h3>Supertypes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="version">
    <div id="version">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="valid-context">
    <h3>Valid Context</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="binding-types">
    <h3>Binding Types Affected</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="syntax">
    <h3>Syntax</h3>
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

  <xsl:template match="arguments">
    <h3>Arguments</h3>
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
    <h3>Arguments</h3>
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

  <xsl:template match="affected-by">
    <div class="noindent">
      <h3>Affected By</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="examples">
    <div class="noindent">
      <h3>Examples</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="exceptional">
    <div class="noindent">
      <h3>Exceptional Situations</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="notes">
    <div class="noindent">
      <h3>Notes</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subsection">
    <div class="noindent">
      <h4><xsl:value-of select="@subsection"/></h4>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subsubsection">
    <div class="noindent">
      <h5><xsl:value-of select="@subsubsection"/></h5>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="methods">
    <div class="noindent">
      <h3>Methods</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <!-- End of Dictionary entries -->

  <xsl:template match="about-symbol">
    <xsl:for-each select="key('symbol-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kind-name"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
          </xsl:when>
            <xsl:otherwise>
            <xsl:apply-templates select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-variable">
    <xsl:for-each select="key('variable-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          Variable
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-function">
    <xsl:for-each select="key('function-by-name', text())">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Function'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-generic">
    <xsl:for-each select="key('generic-by-name', text())">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Generic Function'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-operator">
    <xsl:for-each select="key('operator-by-name', text())">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Special Operator'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-macro">
    <xsl:for-each select="key('macro-by-name', text())">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Macro'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-type">
    <xsl:for-each select="key('type-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          Type
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
          </xsl:when>
            <xsl:otherwise>
            <xsl:apply-templates select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-class">
    <xsl:for-each select="key('class-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          Class
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-systemclass">
    <xsl:for-each select="key('systemclass-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          System Class
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-struct">
    <xsl:for-each select="key('struct-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          Struct
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-condition">
    <xsl:for-each select="key('condition-by-name', text())">
      <div class="def">
        <a href="{@id}.html">
          Condition Type
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
        (undocumented)
      </span>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
