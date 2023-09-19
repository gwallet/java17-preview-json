package kawa.json;

import java.io.Serial;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Objects;
import java.util.Optional;
import java.util.Spliterator;
import java.util.function.BiConsumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

/**
 * Stupid simple JSON facility.
 * Conforms to <a href="https://www.ietf.org/rfc/rfc4627.txt?number=4627">RFC 4627</a>.
 */
public final class Json {

  public interface Parser {

    Value parse() throws Exception;

    class Exception extends RuntimeException {

      @Serial private static final long serialVersionUID = -7034897190745766939L;

      public Exception() {
        super();
      }

      public Exception(java.lang.String message) {
        super(message);
      }

      public Exception(java.lang.String message, Throwable cause) {
        super(message, cause);
      }

      public Exception(Throwable cause) {
        super(cause);
      }

      protected Exception(java.lang.String message,
                          Throwable cause,
                          boolean enableSuppression,
                          boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
      }

    }

  }

  public sealed interface Value {

    interface Visitor {

      default void visit(Object object) {}

      default void visit(Array array) {}

      default void visit(String string) {}

      default void visit(Number number) {}

      default void visit(Boolean bool) {}

      default void visit(Null nil) {}

    }

    default void accept(Visitor visitor) {
      //  Ensure visitors have all `visit` methods
      switch (this) {
        // @formatter:off
        case Object object -> visitor.visit(object);
        case Array   array -> visitor.visit(array);
        case String string -> visitor.visit(string);
        case Number number -> visitor.visit(number);
        case Boolean  bool -> visitor.visit(bool);
        case Null      nil -> visitor.visit(nil);
        // @formatter:on
      }
    }

  }

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
  public record Object(Member... members) implements Value, Iterable<Object.Member> {

    @Override public Iterator<Member> iterator() {
      return Arrays.stream(members).iterator();
    }

    @Override public Spliterator<Member> spliterator() {
      return Arrays.spliterator(members);
    }

    public void forEach(BiConsumer<java.lang.String, Value> consumer) {
      this.forEach(member -> consumer.accept(member.key.value, member.value));
    }

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

      @Override public boolean equals(java.lang.Object obj) {
        if (obj instanceof Member that) {
          return Objects.equals(this.key, that.key)
                 && Objects.equals(this.value, that.value);
        }

        return false;
      }

      @Override public int hashCode() {
        return Objects.hash(key, value);
      }

    }

    /**
     * Creates a new {@code Json.Object}, clone of this with one more property.
     *
     * @param key   new property name.
     * @param value new property value.
     *
     * @return Returns a new {@code Json.Object}, clone of this with one more property in the end.
     *
     * @throws java.lang.NullPointerException if {@code key} or {@code value} was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Object with(java.lang.String key, Value value) {
      return with(Json.string(key), value);
    }

    /**
     * Creates a new {@code Json.Object}, clone of this with one more property.
     *
     * @param key   new property name.
     * @param value new property value.
     *
     * @return Returns a new {@code Json.Object}, clone of this with one more property in the end.
     *
     * @throws java.lang.NullPointerException if {@code key} or {@code value} was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Object with(String key, Value value) {
      Member[] m = new Member[members.length + 1];
      System.arraycopy(members, 0, m, 0, members.length);
      m[members.length] = new Object.Member(Objects.requireNonNull(key, "key must not be null."),
                                            Objects.requireNonNull(value, "value must not be null, see Json.NULL#or(Json.Value)"));
      return new Object(m);
    }

    public Value get(java.lang.String key) {
      return _get(key).orElse(Json.NULL);
    }

    private Optional<Value> _get(java.lang.String key) {
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

    @Override public boolean equals(java.lang.Object obj) {
      if (obj instanceof Object that) {
        return Arrays.equals(this.members, that.members);
      }

      return false;
    }

    @Override public int hashCode() {
      return Arrays.hashCode(this.members);
    }

    @Override public java.lang.String toString() {
      return Arrays.stream(members)
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

    private java.lang.String toPrettyString(java.lang.String indent, java.lang.String indentation) {
      if (isEmpty()) {
        return "{}";
      }

      // @formatter:off
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
      // @formatter:on
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
  public record Array(Value... values) implements Value, Iterable<Value> {

    @Override public Iterator<Value> iterator() {
      return Arrays.stream(values).iterator();
    }

    @Override public Spliterator<Value> spliterator() {
      return Arrays.spliterator(values);
    }

    /**
     * Creates a new {@code Json.Array} clone of this, but with one more element in the end.
     *
     * @param value New value to append in the end of this {@code Json.Array}.
     *
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
     *
     * @return Returns a new {@code Json.Array} clone of this, but with all elements in the end.
     *
     * @throws java.lang.NullPointerException if one of the given values was null.
     * To avoid null {@code value}, you can use {@code Json.NULL.or(nullableValue)} which deals with null safety.
     */
    public Array with(Value... values) {
      // @formatter:off
      IntStream.range(0, values.length)
               .forEach(index -> Objects.requireNonNull(values[index], "Expecting non-null values in array at index " + index));
      // @formatter:on
      Value[] v = new Value[this.values.length + values.length];
      System.arraycopy(this.values, 0, v, 0, this.values.length);
      System.arraycopy(values, 0, v, this.values.length, values.length);
      return new Array(v);
    }

    public boolean isEmpty() {
      return values == null || values.length == 0;
    }

    @Override public boolean equals(java.lang.Object obj) {
      if (obj instanceof Array that) {
        return Arrays.equals(this.values, that.values);
      }

      return false;
    }

    @Override public int hashCode() {
      return Arrays.hashCode(this.values);
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

    private java.lang.String toPrettyString(java.lang.String indent, java.lang.String indentation) {
      if (isEmpty()) {
        return "[]";
      }

      // @formatter:off
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
      // @formatter:on
    }

  }

  /**
   * @return Creates and returns a new {@code Json.String}.
   */
  public static String string(java.lang.String string) {
    return new String(Objects.requireNonNull(string, "string must not be null."));
  }

  /**
   * Represents a {@code Json.String}.
   */
  public record String(java.lang.String value) implements Value {

    @Override public java.lang.String toString() {
      return "\"" + value + "\"";
    }

    @Override public boolean equals(java.lang.Object obj) {
      if (obj instanceof String that) {
        return Objects.equals(this.value, that.value);
      }

      return false;
    }

    @Override public int hashCode() {
      return Objects.hashCode(value);
    }

  }

  /**
   * @return Creates and returns a new {@code Json.Number}.
   */
  public static Number number(java.lang.Number n) {
    if (n instanceof Integer i) {
      return new Number((long) i);
    }
    if (n instanceof Float f) {
      return new Number((double) f);
    }
    return new Number(Objects.requireNonNull(n, "number must not be null"));
  }

  /**
   * Represents a {@code Json.Number} backed by a {@code java.lang.Number}.
   */
  public record Number(java.lang.Number value) implements Value {

    @Override public java.lang.String toString() {
      return "" + value;
    }

    @Override public boolean equals(java.lang.Object obj) {
      if (obj instanceof Number that) {
        return Objects.equals(this.value, that.value);
      }

      return false;
    }

    @Override public int hashCode() {
      return Objects.hashCode(value);
    }

  }

  public static Boolean bool(boolean bool) {
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
      if (obj instanceof Boolean that) {
        return this.value == that.value;
      }

      return false;
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
