package kawa.json;

import kawa.json.parser.javacc.JavaccJsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

public class TestJsonParser {

  @ParameterizedTest(name = "should read {arguments} as valid JSON value")
  @MethodSource("simple_input_output")
  void should_read_input_as_valid_JSON(String source, Json.Value expectedValue) throws Exception {
      // Given
      Json.Parser parser = new JavaccJsonParser(source);

      // When
      Json.Value value = null;
      try {
          value = parser.parse();
      }
      catch (Json.Parser.Exception cause) {
        fail("Can't parse '" + source + "'", cause);
      }

      // Then
      assertThat(value)
              .isEqualTo(expectedValue);
  }

  public static Stream<Arguments> simple_input_output() {
    return Stream.of(
      Arguments.of("[]", Json.array()),
      Arguments.of("{}", Json.object()),
      Arguments.of("0.42", Json.number(0.42)),
      Arguments.of("-42.0", Json.number(-42.0)),
      Arguments.of("-42", Json.number(-42L)),
      Arguments.of("42.0", Json.number(42.0)),
      Arguments.of("42", Json.number(42L)),
      Arguments.of("\"string\"", Json.string("string")),
      Arguments.of("true", Json.TRUE),
      Arguments.of("false", Json.FALSE),
      Arguments.of("null", Json.NULL)
    );
  }

  @Test void should_read_object_as_valid_JSON() throws Exception {
    // Given
    final String source =
      """
      {
        "object": {},
        "array": [],
        "title": "Doctor Who?",
        "number": 42,
        "boolean": false,
        "null": null
      }
      """;
    Json.Parser parser = new JavaccJsonParser(source);
    Json.Object expectedValue =
      Json.object()
          .with("object",  Json.NULL.or(Json.object()))
          .with("array",   Json.NULL.or(Json.array()))
          .with("title",   Json.NULL.or(Json.string("Doctor Who?")))
          .with("number",  Json.NULL.or(Json.number(42)))
          .with("boolean", Json.NULL.or(Json.FALSE.or(Json.FALSE.and(Json.TRUE))))
          .with("null",    Json.NULL.or(null));


    // When
    Json.Value value = null;
    try {
      value = parser.parse();
    }
    catch (Json.Parser.Exception cause) {
      fail("Can't parse '" + source + "'", cause);
    }

    // Then
    assertThat(value)
      .isEqualTo(expectedValue);
  }

  @Test void should_read_array_as_valid_JSON() throws Exception {
    // Given
    final String source =
      """
      [
        {},
        [],
        "Doctor Who?",
        42,
        false,
        null
      ]
      """;
    Json.Parser parser = new JavaccJsonParser(source);
    final Json.Value expectedValue = Json.array().with(
      Json.object(),
      Json.array(),
      Json.string("Doctor Who?"),
      Json.number(42),
      Json.FALSE,
      Json.NULL
    );

    // When
    Json.Value value = null;
    try {
      value = parser.parse();
    }
    catch (Json.Parser.Exception cause) {
      fail("Can't parse '" + source + "'", cause);
    }

    // Then
    assertThat(value)
      .isEqualTo(expectedValue);
  }

}
