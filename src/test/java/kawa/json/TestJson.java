package kawa.json;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class TestJson {

  @Test void should_write_empty_object() throws Exception {
    assertThat(Json.object().toString(false))
      .isEqualTo("{}");
    assertThat(Json.object().toString(true))
      .isEqualTo("{}");
    assertThat(Json.object().isEmpty())
      .isTrue();
  }

  @Test void should_write_empty_array() throws Exception {
    assertThat(Json.array().toString(false))
      .isEqualTo("[]");
    assertThat(Json.array().toString(true))
      .isEqualTo("[]");
    assertThat(Json.array().isEmpty())
      .isTrue();
  }

  @Test void should_write_null() throws Exception {
    assertThat(Json.NULL.toString())
      .isEqualTo("null");

    assertThat(Json.array().with(Json.NULL).toString())
      .isEqualTo("[null]");

    assertThat(Json.object().with("null", Json.NULL).toString())
      .isEqualTo("{\"null\":null}");
  }

  @Test void should_write_string() throws Exception {
    assertThat(Json.string("Doctor Who?").toString())
      .isEqualTo("\"Doctor Who?\"");

    assertThat(Json.array().with(new Json.String("Doctor Who?")).toString())
      .isEqualTo("[\"Doctor Who?\"]");

    assertThat(Json.object().with("title", Json.string("Doctor Who?")).toString())
      .isEqualTo("{\"title\":\"Doctor Who?\"}");
  }

  @Test void should_write_boolean() throws Exception {
    assertThat(Json.TRUE.toString())
      .isEqualTo("true");
    assertThat(Json.FALSE.toString())
      .isEqualTo("false");
    assertThat(Json.bool(true).toString())
      .isEqualTo("true");
    assertThat(Json.bool(false).toString())
      .isEqualTo("false");

    assertThat(Json.array().with(Json.TRUE, Json.FALSE).toString())
      .isEqualTo("[true,false]");

    assertThat(Json.object()
                   .with("true", Json.TRUE)
                   .with("false", Json.FALSE)
                   .toString())
      .isEqualTo("{\"true\":true,\"false\":false}");
  }

  @Test void should_write_number() throws Exception {
    assertThat(Json.number(42).toString())
      .isEqualTo("42");

    assertThat(Json.array().with(Json.number(42)).toString())
      .isEqualTo("[42]");

    assertThat(Json.object().with("forty-two", new Json.Number(42)).toString())
      .isEqualTo("{\"forty-two\":42}");
  }

  @Test void should_access_object_members() throws Exception {
    {
      Json.Object object = Json.object()
                               .with("object",  Json.NULL.or(Json.object()))
                               .with("array",   Json.NULL.or(Json.array()))
                               .with("title",   Json.NULL.or(Json.string("Doctor Who?")))
                               .with("number",  Json.NULL.or(Json.number(42)))
                               .with("boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))))
                               .with("null",    Json.NULL.or(null));
      assertThat(object.isEmpty())
        .isFalse();
      object.forEach((key, value) -> {
        switch (value) {
          case Json.Object  o -> { assertThat(key).isEqualTo("object");  assertThat(o).isEmpty(); }
          case Json.Array   a -> { assertThat(key).isEqualTo("array");   assertThat(a).isEmpty(); }
          case Json.String  s -> { assertThat(key).isEqualTo("title");   assertThat(s).isEqualTo(Json.string("Doctor Who?")); }
          case Json.Number  n -> { assertThat(key).isEqualTo("number");  assertThat(n).isEqualTo(Json.number(42)); }
          case Json.Boolean b -> { assertThat(key).isEqualTo("boolean"); assertThat(b).isEqualTo(Json.FALSE); }
          case Json.Null    ø -> { assertThat(key).isEqualTo("null");    assertThat(ø).isEqualTo(Json.NULL); }
        }
      });
    }

    {
      Map<String, Json.Value> members = Map.of("object",  Json.NULL.or(Json.object()),
                                               "array",   Json.NULL.or(Json.array()),
                                               "title",   Json.NULL.or(Json.string("Doctor Who?")),
                                               "number",  Json.NULL.or(Json.number(42)),
                                               "boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))),
                                               "null",    Json.NULL.or(null));
      Json.Object object = members.entrySet().stream().parallel()
                                  .reduce(Json.object(),
                                          (Json.Object o, Map.Entry<String, Json.Value> e) -> o.with(e.getKey(), e.getValue()),
                                          Json.Object::concat);

      assertThat(object.getObject("object")).isEmpty();
      assertThat(object.getArray("array")).isEmpty();
      assertThat(object.getString("title")).isEqualTo(Json.string("Doctor Who?"));
      assertThat(object.getNumber("number")).isEqualTo(Json.number(42));
      assertThat(object.getBoolean("boolean")).isEqualTo(Json.FALSE);
      assertThat(object.get("null")).isEqualTo(Json.NULL);
    }
  }

  @Test void should_pretty_print_JSON_array() throws Exception {
    Json.String X = Json.string("X");
    Json.String O = Json.string("O");
    Json.Array ticTacToe = Json.array().with(
      Json.array().with(X, O, X),
      Json.array().with(O, X, O),
      Json.array().with(X, O, X)
    );
    assertThat(ticTacToe.toString(true))
      .isEqualTo("""
                   [
                     [
                       "X",
                       "O",
                       "X"
                     ],
                     [
                       "O",
                       "X",
                       "O"
                     ],
                     [
                       "X",
                       "O",
                       "X"
                     ]
                   ]""");
  }

  @Test void should_pretty_print_JSON_document() throws Exception {
    Json.Object object = Json.object()
                             .with("object",  Json.NULL.or(Json.object().with("sub-title", Json.string("EXTERMINATE!"))))
                             .with("array",   Json.NULL.or(Json.array().with(Json.string("T"),
                                                                             Json.string("A"),
                                                                             Json.string("R"),
                                                                             Json.string("D"),
                                                                             Json.string("I"),
                                                                             Json.string("S"))))
                             .with("kv",      Json.NULL.or(Json.array().with(Json.object().with("k", Json.string("key")).with("v", Json.string("value")),
                                                                             Json.object().with("k", Json.string("king")).with("v", Json.string("void")))))
                             .with("title",   Json.NULL.or(Json.string("Doctor Who?")))
                             .with("number",  Json.NULL.or(Json.number(42)))
                             .with("boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))))
                             .with("null",    Json.NULL.or(null));
    assertThat(object.toString(true))
      .isEqualTo("""
                   {
                     "object": {
                       "sub-title": "EXTERMINATE!"
                     },
                     "array": [
                       "T",
                       "A",
                       "R",
                       "D",
                       "I",
                       "S"
                     ],
                     "kv": [
                       {
                         "k": "key",
                         "v": "value"
                       },
                       {
                         "k": "king",
                         "v": "void"
                       }
                     ],
                     "title": "Doctor Who?",
                     "number": 42,
                     "boolean": false,
                     "null": null
                   }""");
  }

  @Test void should_iterate_over_array_values() throws Exception {
    Json.Array array = Json.array()
                           .with(
                             Json.object(),
                             Json.array(),
                             Json.string("Doctor Who?"),
                             Json.number(42),
                             Json.TRUE.or(Json.FALSE).and(Json.TRUE),
                             Json.NULL
                           );
    assertThat(array.isEmpty())
      .isFalse();
    array.forEach(value -> {
      switch (value) {
        case Json.Object  o -> assertThat(o).isEmpty();
        case Json.Array   a -> assertThat(a).isEmpty();
        case Json.String  s -> assertThat(s).isEqualTo(Json.string("Doctor Who?"));
        case Json.Number  n -> assertThat(n).isEqualTo(Json.number(42));
        case Json.Boolean b -> assertThat(b).isEqualTo(Json.TRUE);
        case Json.Null    ø -> assertThat(ø).isEqualTo(Json.NULL);
      }
    });
  }

}
