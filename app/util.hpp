#pragma once
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_INITIAL 32
#define ARRAY_AGGRESSION 2

template <typename T> class Maybe {
private:
  T value;
  uint8_t ok;
  Maybe(T value) : ok(1), value(value) { }

public:
  Maybe() : ok(0) { }

  static Maybe<T> None() { return Maybe<T>(); }
  static Maybe<T> Some(T value) { return Maybe<T>(value); }

  uint8_t is_some() { return ok; }
  T unwrap() { return value; }
};

template <typename T> class Array {
private:
  T *buffer;
  uint32_t size;
  uint32_t alloc_size;
public:
  Array();
  ~Array();

  void push(T value);
  Maybe<T> get(uint32_t n);
  Maybe<T*> grab(uint32_t n);
};

template <typename T> Array<T>::Array() {
  this->alloc_size = ARRAY_INITIAL;

  this->buffer = (T *)calloc(this->alloc_size, sizeof(T));
  this->size = 0;
}

template <typename T> Array<T>::~Array() {
  free(this->buffer);
  this->alloc_size = 0;
  this->size = 0;
}

template <typename T> void Array<T>::push(T value) {
  buffer[this->size] = value;

  this->size++;

  if (this->size >= this->alloc_size) {
    this->alloc_size *= ARRAY_AGGRESSION;

    if (!realloc(buffer, sizeof(T) * this->alloc_size)) {
      exit(137);
    }
  }
}


template <typename T> Maybe<T*> Array<T>::grab(uint32_t n) {
  if (n < this->size) {
    return Maybe<T*>::Some(this->buffer + n);
  }
  
  return Maybe<T*>::None();
}

template <typename T> Maybe<T> Array<T>::get(uint32_t n) {
  Maybe<T *> grabbed = this->grab(n);

  if (grabbed.is_some()) {
    return Maybe<T>::Some(*grabbed.unwrap());
  }

  return Maybe<T>::None();
}
