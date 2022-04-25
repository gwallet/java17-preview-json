package kawa.json;

import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

/**
 * Stupid simple JSON facility.
 * Conforms to <a href="https://www.ietf.org/rfc/rfc4627.txt?number=4627">RFC 4627</a>.
 */
public final class Json {

  sealed interface Value {}

  /**
   * @return Creates and returns a new empty {@code Json.Object} document.
   *
   * @see kawa.json.Json.Object#with(java.lang.String, kawa.json.Json.Value)
   */
  public static Object object() {
    return new Json.Object();
  }

  /**
   * Represents a JSON document containing {@code key -> value} pairs.
   */
  public record Object(Member... members) implements Value {

    public static Object concat(Object a, Object b) {
      Member[] m = new Member[a.members.length + b.members.length];
      System.arraycopy(a.members, 0, m, 0, a.members.length);
      System.arraycopy(b.members, 0, m, a.members.length, b.members.length);
      return new Object(m);
    }

    /**
     * Represents a {@code key -> value} pair part of a JSON document.
     */
    public record Member(String key, Value value) {

      @Override public java.lang.String toString() {
        return key + ":" + value.toString();
      }

    }

    /**
     * Creates a new {@code Json.Object}, clone of this with one more property.
     *
     * @param key   new property name.
     * @param value new property value.
     * @return Returns a new {@code Json.Object}, clone of this with one more property in the end.
     *
     * @throws java.lang.NullPointerException if {@code key} or {@code value} was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Object with(java.lang.String key, Value value) {
      Member[] m = new Member[members.length + 1];
      System.arraycopy(members, 0, m, 0, members.length);
      m[members.length] = new Object.Member(new String(Objects.requireNonNull(key, "key must not be null.")),
                                            Objects.requireNonNull(value, "value must not be null, see Json.NULL#or(Json.Value)"));
      return new Object(m);
    }

    public Value get(java.lang.String key) {
      return _get(key).orElse(Json.NULL);
    }

    public Optional<Value> _get(java.lang.String key) {
      return Stream.of(members)
                   .filter(member -> member.key.equals(Json.string(key)))
                   .findFirst()
                   .map(Member::value);
    }

    private <V extends Value> Optional<V> _get(java.lang.String key, Class<V> type) {
      return _get(key).filter(v -> v.getClass().isAssignableFrom(type))
                     .map(type::cast);
    }

    public Json.Object getObject(java.lang.String key) {
      return _get(key, Json.Object.class)
        .orElse(null);
    }

    public Json.Array getArray(java.lang.String key) {
      return _get(key, Json.Array.class)
        .orElse(null);
    }

    public Json.String getString(java.lang.String key) {
      return _get(key, Json.String.class)
        .orElse(null);
    }

    public Json.Integer getInteger(java.lang.String key) {
      return _get(key, Json.Integer.class)
        .orElse(null);
    }

    public Json.Number getNumber(java.lang.String key) {
      return _get(key, Json.Number.class)
        .orElse(null);
    }

    public Json.Boolean getBoolean(java.lang.String key) {
      return _get(key, Json.Boolean.class)
        .orElse(null);
    }

    public boolean isEmpty() {
      return members == null || members.length == 0;
    }

    public void forEach(BiConsumer<java.lang.String, Value> consumer) {
      Stream.of(members)
            .forEach(member -> consumer.accept(member.key.value, member.value));
    }

    @Override public java.lang.String toString() {
      return Stream.of(members)
                   .map(Member::toString)
                   .collect(joining(",", "{", "}"));
    }

    public java.lang.String toString(boolean pretty) {
      if (pretty) {
        return toPrettyString("", "  ");
      }
      else {
        return toString();
      }
    }

    java.lang.String toPrettyString(java.lang.String indent, java.lang.String indentation) {
      if (isEmpty()) {
        return "{}";
      }

      return Stream.of(members)
            .map(member -> {
              StringBuilder buffer = new StringBuilder();
              switch (member.value) {
                case Array  a -> buffer.append(indent).append(indentation).append(member.key).append(": ").append(a.toPrettyString(indent + indentation, indentation));
                case Object o -> buffer.append(indent).append(indentation).append(member.key).append(": ").append(o.toPrettyString(indent + indentation, indentation));
                default -> buffer.append(indent).append(indentation).append(member.key).append(": ").append(member.value);
              }
              return buffer.toString();
            })
        .collect(joining(",\n", "{\n", "\n" + indent + "}"));
    }

  }

  /**
   * @return Creates and returns a new empty {@code Json.Array}.
   */
  public static Array array() {
    return new Array();
  }

  /**
   * Represents an array of {@code Json.Value}s.
   */
  public record Array(Value... values) implements Value {

    /**
     * Creates a new {@code Json.Array} clone of this, but with one more element in the end.
     *
     * @param value New value to append in the end of this {@code Json.Array}.
     * @return Returns a new {@code Json.Array} clone of this, but with one more element in the end.
     *
     * @throws java.lang.NullPointerException if the given value was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Array with(Value value) {
      Value[] v = new Value[values.length + 1];
      System.arraycopy(values, 0, v, 0, values.length);
      v[values.length] = Objects.requireNonNull(value, "The new value must not be null");
      return new Array(v);
    }

    /**
     * Creates a new {@code Json.Array} clone of this, but with one more element in the end.
     *
     * @param values New values to append in the end of this {@code Json.Array}.
     * @return Returns a new {@code Json.Array} clone of this, but with all elements in the end.
     *
     * @throws java.lang.NullPointerException if one of the given values was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Array with(Value... values) {
      IntStream.range(0, values.length)
               .forEach(index -> Objects.requireNonNull(values[index], "Expecting non-null values in array at index " + index));
      Value[] v = new Value[this.values.length + values.length];
      System.arraycopy(this.values, 0, v, 0, this.values.length);
      System.arraycopy(values, 0, v, this.values.length, values.length);
      return new Array(v);
    }

    public boolean isEmpty() {
      return values == null || values.length == 0;
    }

    public void forEach(Consumer<Value> consumer) {
      Stream.of(values)
            .forEach(consumer);
    }

    @Override public java.lang.String toString() {
      return Stream.of(values)
                   .map(Value::toString)
                   .collect(joining(",", "[", "]"));
    }

    public java.lang.String toString(boolean pretty) {
      if (pretty) {
        return toPrettyString("", "  ");
      }
      else {
        return toString();
      }
    }

    java.lang.String toPrettyString(java.lang.String indent, java.lang.String indentation) {
      if (isEmpty()) {
        return "[]";
      }

      return Stream.of(values)
                   .map(value -> {
                     StringBuilder buffer = new StringBuilder();
                     switch (value) {
                       case Array  a -> buffer.append(indent).append(indentation).append(a.toPrettyString(indent + indentation, indentation));
                       case Object o -> buffer.append(indent).append(indentation).append(o.toPrettyString(indent + indentation, indentation));
                       default -> buffer.append(indent).append(indentation).append(value);
                     }
                     return buffer.toString();
                   })
                   .collect(joining(",\n", "[\n", "\n" + indent + "]"));
    }

  }

  /**
   * @return Creates and returns a new {@code Json.String}.
   */
  public static String string(java.lang.String string) {
    return new String(string);
  }

  /**
   * Represents a {@code Json.String}.
   */
  public record String(java.lang.String value) implements Value {

    @Override public java.lang.String toString() {
      return "\"" + value + "\"";
    }

  }

  /**
   * @return Creates and returns a new {@code Json.Integer}.
   */
  public static Integer integer(int i) {
    return new Integer(i);
  }

  /**
   * Represents a {@code Json.Integer}.
   */
  public record Integer(int value) implements Value {

    @Override public java.lang.String toString() {
      return "" + value;
    }

  }

  /**
   * @return Creates and returns a new {@code Json.Number}.
   */
  public static Number number(float f) {
    return new Number(f);
  }

  /**
   * Represents a {@code Json.Number} backed by a {@code float}.
   */
  public record Number(float value) implements Value {

    @Override public java.lang.String toString() {
      return "" + value;
    }

  }

  public static Boolean booleanValueOf(boolean bool) {
    return bool
      ? TRUE
      : FALSE;
  }

  public static final Boolean TRUE = new Boolean(true);

  public static final Boolean FALSE = new Boolean(false);

  /**
   * Represents a {@code Json.Boolean} value.
   */
  public record Boolean(boolean value) implements Value {

    public Json.Boolean or(Json.Boolean other) {
      if (value) {
        return this;
      }
      return other;
    }

    public Json.Boolean and(Json.Boolean other) {
      if (value) {
        return other;
      }
      else {
        return this;
      }
    }

    @Override public boolean equals(java.lang.Object obj) {
      return ( obj instanceof Boolean )
             && ( (Boolean) obj ).value == this.value;
    }

    @Override public java.lang.String toString() {
      return "" + value;
    }

  }

  public static final Null NULL = new Null();

  public record Null() implements Value {

    public Value or(Value value) {
      return Objects.requireNonNullElse(value, this);
    }

    @Override public boolean equals(java.lang.Object obj) {
      return obj instanceof Null;
    }

    @Override public java.lang.String toString() {
      return "null";
    }

  }

  private Json() {}

}
