package com.dmtavt.fragpipe.api;

import org.greenrobot.eventbus.EventBus;

public class Bus {
  private static final EventBus b = EventBus.getDefault();

  public static void clearCaches() {
    EventBus.clearCaches();
  }

  public static void register(Object subscriber) {
    b.register(subscriber);
  }

  public static boolean isRegistered(Object subscriber) {
    return b.isRegistered(subscriber);
  }

  public static void unregister(Object subscriber) {
    b.unregister(subscriber);
  }

  public static void post(Object event) {
    b.post(event);
  }

  public static void cancelEventDelivery(Object event) {
    b.cancelEventDelivery(event);
  }

  public static void postSticky(Object event) {
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
