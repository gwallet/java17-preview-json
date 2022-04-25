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
    assertThat(Json.booleanValueOf(true).toString())
      .isEqualTo("true");
    assertThat(Json.booleanValueOf(false).toString())
      .isEqualTo("false");

    assertThat(Json.array().with(Json.TRUE, Json.FALSE).toString())
      .isEqualTo("[true,false]");

    assertThat(Json.object()
                   .with("true", Json.TRUE)
                   .with("false", Json.FALSE)
                   .toString())
      .isEqualTo("{\"true\":true,\"false\":false}");
  }

  @Test void should_write_integer() throws Exception {
    assertThat(Json.integer(42).toString())
      .isEqualTo("42");

    assertThat(Json.array().with(Json.integer(42)).toString())
      .isEqualTo("[42]");

    assertThat(Json.object().with("forty-two", Json.integer(42)).toString())
      .isEqualTo("{\"forty-two\":42}");
  }

  @Test void should_write_float() throws Exception {
    assertThat(Json.number(42).toString())
      .isEqualTo("42.0");

    assertThat(Json.array().with(Json.number(42)).toString())
      .isEqualTo("[42.0]");

    assertThat(Json.object().with("forty-two", new Json.Number(42)).toString())
      .isEqualTo("{\"forty-two\":42.0}");
  }

  @Test void should_access_object_members() throws Exception {
    {
      Json.Object object = Json.object()
                               .with("object",  Json.NULL.or(Json.object()))
                               .with("array",   Json.NULL.or(Json.array()))
                               .with("title",   Json.NULL.or(Json.string("Doctor Who?")))
                               .with("integer", Json.NULL.or(Json.integer(42)))
                               .with("number",  Json.NULL.or(Json.number(42)))
                               .with("boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))))
                               .with("null",    Json.NULL.or(null));
      assertThat(object.isEmpty())
        .isFalse();
      object.forEach((key, value) -> {
        switch (value) {
          case Json.Object  o -> { assertThat(key).isEqualTo("object");  assertThat(o.members()).isEmpty(); }
          case Json.Array   a -> { assertThat(key).isEqualTo("array");   assertThat(a.values()).isEmpty(); }
          case Json.String  s -> { assertThat(key).isEqualTo("title");   assertThat(s.value()).isEqualTo("Doctor Who?"); }
          case Json.Integer i -> { assertThat(key).isEqualTo("integer"); assertThat(i.value()).isEqualTo(42); }
          case Json.Number  n -> { assertThat(key).isEqualTo("number");  assertThat(n.value()).isEqualTo(42.0f); }
          case Json.Boolean b -> { assertThat(key).isEqualTo("boolean"); assertThat(b).isEqualTo(Json.FALSE); }
          case Json.Null    ø -> { assertThat(key).isEqualTo("null");    assertThat(ø).isEqualTo(Json.NULL); }
        }
      });
    }

    {
      Map<String, Json.Value> members = Map.of("object",  Json.NULL.or(Json.object()),
                                               "array",   Json.NULL.or(Json.array()),
                                               "title",   Json.NULL.or(Json.string("Doctor Who?")),
                                               "integer", Json.NULL.or(Json.integer(42)),
                                               "number",  Json.NULL.or(Json.number(42)),
                                               "boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))),
                                               "null",    Json.NULL.or(null));
      Json.Object object = members.entrySet().stream().parallel()
                                  .reduce(Json.object(),
                                          (Json.Object o, Map.Entry<String, Json.Value> e) -> o.with(e.getKey(), e.getValue()),
                                          Json.Object::concat);

      assertThat(object.getObject("object").members()).isEmpty();
      assertThat(object.getArray("array").values()).isEmpty();
      assertThat(object.getString("title").value()).isEqualTo("Doctor Who?");
      assertThat(object.getInteger("integer").value()).isEqualTo(42);
      assertThat(object.getNumber("number").value()).isEqualTo(42.0f);
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
                             .with("integer", Json.NULL.or(Json.integer(42)))
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
                     "integer": 42,
                     "number": 42.0,
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
                             Json.integer(42),
                             Json.number(42),
                             Json.TRUE.or(Json.FALSE).and(Json.TRUE),
                             Json.NULL
                           );
    assertThat(array.isEmpty())
      .isFalse();
    array.forEach(value -> {
      switch (value) {
        case Json.Object  o -> assertThat(o.members()).isEmpty();
        case Json.Array   a -> assertThat(a.values()).isEmpty();
        case Json.String  s -> assertThat(s.value()).isEqualTo("Doctor Who?");
        case Json.Integer i -> assertThat(i.value()).isEqualTo(42);
        case Json.Number  n -> assertThat(n.value()).isEqualTo(42.0f);
        case Json.Boolean b -> assertThat(b).isEqualTo(Json.TRUE);
        case Json.Null    ø -> assertThat(ø).isEqualTo(Json.NULL);
      }
    });
  }

}
