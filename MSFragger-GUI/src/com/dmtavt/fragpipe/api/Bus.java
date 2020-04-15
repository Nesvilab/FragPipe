package com.dmtavt.fragpipe.api;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Bus {
  private static final Logger log = LoggerFactory.getLogger(Bus.class);
  private static final EventBus b;

  static {
    b = EventBus.getDefault();
  }

  public static void clearCaches() {
    EventBus.clearCaches();
  }

  public static void register(Object subscriber) {
    b.register(subscriber);
  }

  public static void registerQuietly(Object subscriber) {
    for (Method m : subscriber.getClass().getMethods()) {
      for (Annotation a : m.getDeclaredAnnotations()) {
        Class<? extends Annotation> aClass = a.annotationType();
        if (org.greenrobot.eventbus.Subscribe.class.equals(aClass)) {
          b.register(subscriber);
          return;
        }
      }
    }
    log.warn("Message for developers. No @Subscribe annotations found on bus subscriber {}", subscriber.getClass().getCanonicalName());
  }

  public static boolean isRegistered(Object subscriber) {
    return b.isRegistered(subscriber);
  }

  public static void unregister(Object subscriber) {
    b.unregister(subscriber);
  }

  public static void post(Object event) {
    log.debug("Posting: {}", event);
    b.post(event);
  }

  public static void cancelEventDelivery(Object event) {
    b.cancelEventDelivery(event);
  }

  public static void postSticky(Object event) {
    log.debug("Posting sticky: {}", event);
    b.postSticky(event);
  }

  public static <T> T getStickyEvent(Class<T> eventType) {
    return b.getStickyEvent(eventType);
  }

  public static <T> T removeStickyEvent(Class<T> eventType) {
    return b.removeStickyEvent(eventType);
  }

  public static boolean removeStickyEvent(Object event) {
    return b.removeStickyEvent(event);
  }

  public static void removeAllStickyEvents() {
    b.removeAllStickyEvents();
  }

  public static boolean hasSubscriberForEvent(Class<?> eventClass) {
    return b.hasSubscriberForEvent(eventClass);
  }
}
