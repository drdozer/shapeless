package shapeless

/**
 * A data range with a sentinel value that indicates no data.
 *
 * Sentinel data types have two disjoint value ranges, `V` and `S` where all values are of type `V` and the sentinel(s)
 * are of type `S`. Sentinel data types are isomporphic to: `V :+: S :+: CNil`.
 *
 * @tparam T the sentineled type
 * @tparam V the non-sentinel value range
 * @tparam S the sentinel value range
 * @author Matthew Pocock
 */
trait Sentinel[T, V, S] {

  /**
   * The equivalent coproduct representation
   */
  // todo: check we've got the order correct here - sentinel values take precedence over normal values
  type coproduct = V :+: S :+: CNil

  /**
   * Check if the value is a sentinel value or not.
   *
   * @param t the value to check
   * @return  true if this value is a sentinal, false otherwise
   */
  def isSentinel(t: T): Boolean

  /**
   * Cast to a value, raising an exception on failure.
   *
   * @param t
   * @return
   */
  def asValue(t: T): V

  /**
   * Apply a function to a value, returning either a value or sentinel.
   *
   * @param t the value to operate on
   * @param f the function to apply
   * @return  the updated value
   */
  def flatMap(t: T, f: V => T): T
}

object Sentinel {

  /**
   * The optional sentinel instance.
   *
   * For some type `V`, the sentinel type is `Option[V]`, the value range is `V` and the sentinel value is `None`.
   * @tparam V
   * @return
   */
  implicit def optionSentinel[V]: Sentinel[Option[V], V, None.type] = new Sentinel[Option[V], V, None.type] {

    override def isSentinel(t: Option[V]) = t.isEmpty

    override def asValue(t: Option[V]) = t.get

    override def flatMap(t: Option[V], f: V => Option[V]) = t flatMap f
  }

  /**
   * Witness for the NaN singleton type.
   */
  // fixme: this feels like implementation detail, not API
  val nanWitness = Witness(Double.NaN)

  // fixme: this is stuff specific to the nanSentinel - should be hidden away in some appropriate scope
  type NaN = nanWitness.T
  type NonNan = Double with ¬[NaN]

  /**
   * The Double.NaN sentinel instance.
   *
   * For doubles, the sentinal value is NaN and the value range is all non-NaN values.
   */
  implicit val nanSentinel: Sentinel[Double, NonNan, NaN] = new Sentinel[Double, NonNan, NaN] {

    override def isSentinel(t: Double) = t.isNaN

    override def asValue(t: Double) =
      if(isSentinel(t)) throw new IllegalArgumentException("Attempted to call asValue on NaN")
      else t.asInstanceOf[NonNan]

    override def flatMap(t: Double, f: NonNan => Double): Double =
      if(isSentinel(t)) nanWitness.value
      else f(t.asInstanceOf[NonNan])
  }

}

trait LowPrioritySentinelInstances {

  /**
   * The nullable sentinel instance.
   *
   * For reference types, `null` is the sentinel value and all non-null values are values.
   *
   * @tparam T  the underlying type
   * @return    a sentinel instance for nullability of `T`
   */
  implicit def nullSentinel[T <: AnyRef]: Sentinel[T, T with NotNull, Null] = new Sentinel[T, T with NotNull, Null] {

    override def isSentinel(t: T) = t == null

    override def asValue(t: T) =
      if(t == null) throw new NullPointerException("Attempted to call asValue on null")
      else t.asInstanceOf[T with NotNull]

    override def flatMap(t: T, f: (T with NotNull) => T) = if(!isSentinel(t)) f(asValue(t)) else null.asInstanceOf[T]
  }

}