/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

/**
 *
 * @author me
 */
public enum MarketIndicator {
    /**
     * The market open for the day.
     */
    OPEN, /**
     * The market close for the day.
     */ CLOSE, /**
     * The volume for the day.
     */ VOLUME, /**
     * The adjusted close. Adjusted for splits and dividends.
     */ ADJUSTED_CLOSE, /**
     * The high for the day.
     */ HIGH, /**
     * The low for the day.
     */ LOW
    
}
