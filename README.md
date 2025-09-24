# ZC8A017 / FloridaString
FloridaString: Flowery StringTemplates for S/4 HANA

## Functions:
1)Шаблонизатор для использования в ERP. Работает на основе SapScript, Web-template, и любых текстовых файлов.
2) DataDrivenTest-формат и шаблонизатор для SAP S/4 HANA.
3) Conversion of num to ordinal string; month num to month-string.

Подробнее
1) Как использовать sap-script тексты ([sapland.ru](https://sappro.sapland.ru/publications/vlozhennie-texti-kak-vozmozhnosti-dlya-kompozitsii-razdeleniya-na-chasti-v-dlinn.html) [dzen](https://dzen.ru/a/ZtsCgjkvRgtUwfHz) [habr](https://habr.com/ru/articles/841422/) )
2) RTTI для заполнение шаблонов длинных текстов ( [sapland.ru](https://sappro.sapland.ru/author-column/21773) [dzen](https://dzen.ru/a/Z27d3qZuuGeS9WFv))
3) [abapClub](https://t.me/ABAPclub) - комментарии и общения + вебинары. [Присоединяйтесь](https://t.me/ABAPclub)

**главный принцип**: это назвать переменные внутри шаблона, сопоставимые с входной структурой. [скриншот](https://github.com/OlegBash599/ZC8A017/blob/main/pict_rtti_sapscript.png). почти как в jinja :-))

------------------
### Добавлена возможно "убирать" неиспользованные переменные из шаблона.
### Добавлены функции для RTTI обработки:
1) VMESTE - если значение пустое, то label тоже не выводится
2) IF_BEG / IF_END - функция условия, который позволяет раздлеить строки шаблона (без промежуточного ELSE). Добавлено просто блоком.
3) FOR_BEG / FOR_END - функция обработки внутренней таблицы и применения контекста к таблицы к блоку
--------------

##### Ссылка на стандартные ресурсы:
1) использование [include subtemplate abap-simple transformation](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenst_tt_include.htm)
2) [xsl-include](https://developer.mozilla.org/en-US/docs/Web/XSLT/Element/include) и [xsl-include-xslt-dev](https://xsltdev.ru/xslt/xsl-include/)

#### notes
Framework is called FloridaString that the strings in framework are "flowery" and "blossom". The framework uses different approaches and technologies and it is like flowers in nature.

#### All Rights Reserved (c)
(Soft is registered and has author)[https://searchplatform.rospatent.gov.ru/doc-tims/RU2025614867ПрЭВМ_20250226]
